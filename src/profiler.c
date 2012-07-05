/* GNU Emacs profiler implementation.

Copyright (C) 2012 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include <stdio.h>
#include <limits.h>
#include <sys/time.h>
#include <signal.h>
#include <setjmp.h>
#include "lisp.h"

struct prof_slot
{
  struct prof_slot *next, *prev;
  Lisp_Object *backtrace;
  unsigned long count;
  unsigned int used : 1;
};

#define PROF_HASH_TABLE_SIZE 128

static Lisp_Object	 prof_mode;
static Lisp_Object	 prof_start_time;
static Lisp_Object	 prof_sample_interval;

static Lisp_Object	*prof_backtrace;
static unsigned int	 prof_max_stack_depth;
static struct prof_slot *prof_hash_table[PROF_HASH_TABLE_SIZE];
static struct prof_slot *prof_slot_heap;
static unsigned int	 prof_slot_heap_size;
static struct prof_slot *prof_free_list;

static unsigned long	 prof_total_count;
static unsigned long     prof_others_count;

static EMACS_UINT
prof_backtrace_hash (Lisp_Object *backtrace)
{
  unsigned int i;
  EMACS_UINT hash = 0;
  for (i = 0; i < prof_max_stack_depth; i++)
    hash = SXHASH_COMBINE (XUINT (backtrace[i]), hash);
  return hash;
}

static unsigned int
prof_backtrace_index (Lisp_Object *backtrace)
{
  return prof_backtrace_hash (backtrace) % PROF_HASH_TABLE_SIZE;
}

static Lisp_Object
prof_backtrace_list (Lisp_Object *backtrace, unsigned int n)
{
  if (n == prof_max_stack_depth || NILP (backtrace[n]))
    return Qnil;
  else
    return Fcons (backtrace[n], prof_backtrace_list (backtrace, n + 1));
}

static unsigned int
prof_backtrace_equal (Lisp_Object *a, Lisp_Object *b)
{
  unsigned int i;
  for (i = 0; i < prof_max_stack_depth; i++)
    {
      if (! EQ (a[i], b[i]))
	return 0;
    }
  return 1;
}

static void
prof_unlink_slot (struct prof_slot *slot)
{
  if (slot->prev)
    slot->prev->next = slot->next;
  else
    prof_hash_table[prof_backtrace_index (slot->backtrace)] = slot->next;
  if (slot->next)
    slot->next->prev = slot->prev;
}

static void
prof_free_slot (struct prof_slot *slot)
{
  eassert (slot->used);
  prof_unlink_slot (slot);
  slot->used = 0;
  slot->next = prof_free_list;
  prof_free_list = slot;
}

static void
prof_evict_slot (struct prof_slot *slot)
{
  prof_others_count += slot->count;
  prof_free_slot (slot);
}

static void
prof_evict_min_slot (void)
{
  unsigned int i;
  struct prof_slot *min = 0;

  for (i = 0; i < prof_slot_heap_size; i++)
    {
      struct prof_slot *s = &prof_slot_heap[i];
      if (! min || (s->used && s->count < min->count))
	min = s;
    }

  prof_evict_slot (min);
}

static struct prof_slot *
prof_allocate_slot (void)
{
  struct prof_slot *slot;

  if (! prof_free_list)
    {
      prof_evict_min_slot ();
      eassert (prof_free_list);
    }
  
  slot = prof_free_list;
  slot->used = 1;
  prof_free_list = prof_free_list->next;

  return slot;
}

static struct prof_slot *
prof_make_slot (Lisp_Object *backtrace)
{
  unsigned int i;
  struct prof_slot *slot = prof_allocate_slot ();
  for (i = 0; i < prof_max_stack_depth; i++)
    slot->backtrace[i] = backtrace[i];
  slot->prev = 0;
  slot->next = 0;
  slot->count = 0;
  return slot;
}

static struct prof_slot *
prof_ensure_slot (Lisp_Object *backtrace)
{
  unsigned int index = prof_backtrace_index (backtrace);
  struct prof_slot *slot = prof_hash_table[index];
  struct prof_slot *prev = slot;
  
  while (slot)
    {
      if (prof_backtrace_equal (backtrace, slot->backtrace))
	goto found;
      prev = slot;
      slot = slot->next;
    }

  slot = prof_make_slot (backtrace);
  if (prev)
    {
      slot->prev = prev;
      prev->next = slot;
    }
  else
    prof_hash_table[index] = slot;

 found:
  return slot;
}

static void
prof_signal_handler (int signal, siginfo_t *info, void *ctx)
{
  unsigned int i;
  struct backtrace *backlist;
  struct prof_slot *slot;

  for (i = 0; i < prof_max_stack_depth; i++)
    prof_backtrace[i] = Qnil;

  for (i = 0, backlist = backtrace_list;
       i < prof_max_stack_depth && backlist;
       i++, backlist = backlist->next)
    {
      Lisp_Object function = *backlist->function;
      /*if (FUNCTIONP (function))*/
      if (SYMBOLP (function) && !NILP (Ffboundp (function)))
	prof_backtrace[i] = function;
    }

  if (! NILP (prof_backtrace[0]))
    {
      slot = prof_ensure_slot (prof_backtrace);
      slot->count++;
      prof_total_count++;
    }
}

static void
prof_block_signal (void)
{
  sigset_t sigset;
  sigemptyset (&sigset);
  sigaddset (&sigset, SIGPROF);
  sigprocmask (SIG_BLOCK, &sigset, 0);
}

static void
prof_unblock_signal (void)
{
  sigset_t sigset;
  sigemptyset (&sigset);
  sigaddset (&sigset, SIGPROF);
  sigprocmask (SIG_UNBLOCK, &sigset, 0);
}

static void
prof_init (void)
{
  unsigned int i;
  struct prof_slot *free_list;

  prof_max_stack_depth = profiler_max_stack_depth;
  prof_backtrace =
    (Lisp_Object *) xmalloc (sizeof (Lisp_Object) * prof_max_stack_depth);

  for (i = 0; i < PROF_HASH_TABLE_SIZE; i++)
    prof_hash_table[i] = 0;

  prof_slot_heap_size = profiler_slot_heap_size;
  prof_slot_heap =
    (struct prof_slot *) xmalloc (sizeof (struct prof_slot)
				  * prof_slot_heap_size);
  for (i = 0; i < prof_slot_heap_size; i++)
    {
      struct prof_slot *s = &prof_slot_heap[i];
      s->used = 0;
      s->backtrace =
	(Lisp_Object *) xmalloc (sizeof (Lisp_Object)
				 * prof_max_stack_depth);
    }

  free_list = prof_free_list = prof_slot_heap;
  for (i = 1; i < prof_slot_heap_size; i++)
    {
      free_list->next = &prof_slot_heap[i];
      free_list = free_list->next;
    }
  free_list->next = 0;

  prof_total_count = 0;
  prof_others_count = 0;
}

DEFUN ("profiler-mode", Fprofiler_mode, Sprofiler_mode, 0, 0, 0,
       doc: /* TODO */)
  (void)
{
  return prof_mode;
}

DEFUN ("profiler-start-sampling",
       Fprofiler_start_sampling, Sprofiler_start_sampling,
       1, 1, 0,
       doc: /* TODO */)
  (Lisp_Object sample_interval)
{
  struct sigaction sa;
  struct itimerval timer;

  if (! prof_slot_heap)
    prof_init ();

  prof_mode = intern ("sample");
  prof_start_time = Fcurrent_time ();
  prof_sample_interval = sample_interval;
  
  sa.sa_sigaction = prof_signal_handler;
  sa.sa_flags = SA_RESTART | SA_SIGINFO;
  sigemptyset (&sa.sa_mask);
  sigaction (SIGPROF, &sa, NULL);

  timer.it_interval.tv_sec = 0;
  timer.it_interval.tv_usec = XINT (sample_interval) * 1000;
  timer.it_value = timer.it_interval;
  setitimer (ITIMER_PROF, &timer, 0);

  return Qt;
}

DEFUN ("profiler-stop-sampling",
       Fprofiler_stop_sampling, Sprofiler_stop_sampling,
       0, 0, 0,
       doc: /* TODO */)
  (void)
{
  setitimer (ITIMER_PROF, NULL, 0);
}

DEFUN ("profiler-reset", Fprofiler_reset, Sprofiler_reset, 0, 0, "",
       doc: /* TODO */)
  (void)
{
  prof_block_signal ();

  if (prof_slot_heap)
    {
      unsigned int i;

      xfree (prof_backtrace);
      prof_backtrace = 0;

      for (i = 0; i < prof_slot_heap_size; i++)
	xfree (prof_slot_heap[i].backtrace);
      xfree (prof_slot_heap);
      prof_slot_heap = 0;
    }

  prof_unblock_signal ();
}

DEFUN ("profiler-take-snapshot",
       Fprofiler_take_snapshot, Sprofiler_take_snapshot,
       0, 0, 0,
       doc: /* TODO */)
  (void)
{
  Lisp_Object stop_time;
  Lisp_Object slots;
  Lisp_Object snapshot;
  unsigned int i;
  
  prof_block_signal ();

  stop_time = Fcurrent_time ();

  slots = list1 (list2 (list1 (Qt), make_number (prof_others_count)));
  for (i = 0; i < prof_slot_heap_size; i++)
    {
      struct prof_slot *s = &prof_slot_heap[i];
      if (s->used)
	{
	  Lisp_Object slot = list2 (prof_backtrace_list (s->backtrace, 0),
				    make_number (s->count));
	  slots = Fcons (slot, slots);
	}
    }

  {
    Lisp_Object args[12] = {
      intern ("mode"),		    prof_mode,
      intern ("start-time"),	    prof_start_time,
      intern ("stop-time"),	    stop_time,
      intern ("slots"),		    slots,
      intern ("sample-interval"),   prof_sample_interval,
      intern ("sample-count"),	    make_number (prof_total_count),
    };
    snapshot = Flist (sizeof (args) / sizeof (Lisp_Object), args);
  }

  prof_unblock_signal ();

  return snapshot;
}

void
syms_of_profiler (void)
{
  prof_mode = Qnil;

  profiler_max_stack_depth = 16;
  DEFVAR_INT ("profiler-max-stack-depth", profiler_max_stack_depth,
	      doc: /* TODO */);
  profiler_slot_heap_size = 10000;
  DEFVAR_INT ("profiler-slot-heap-size", profiler_slot_heap_size,
	      doc: /* TODO */);
  
  defsubr (&Sprofiler_mode);
  defsubr (&Sprofiler_start_sampling);
  defsubr (&Sprofiler_stop_sampling);
  defsubr (&Sprofiler_reset);
  defsubr (&Sprofiler_take_snapshot);
}
