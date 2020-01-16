Modules
=======
.. _md_program:

mod_calendar
------------

The module mod_calendar corrects the calendar of the simulation  using the given time step and the initial date. The calendar is computed for both forward and backward simulations in time.

Let us consider a simulation forward in time, where the start year (StartYear) is set to 2000 and the current year (currYear) is 2004. The variable iyear describes the number of simulation years. If loopYears is activated, after reaching the year set as loopEndYear (2010 in this case) the calendar is set back to the loopStartYear (2000). iyear is not altered by this correction.

.. image:: figs/fig_calendar.pdf
    :width: 700px
    :align: center
    :height: 600px
    :alt: alternate text
.. warning::  Note than in backwar simulations in time, loopStartYear>loopEndYear. The start year (StartYear) is always larger or equal to the current year (currYear) - unless loopYears is activated.  If loopYears is activated, the calendar is corrected in similar way to the forward simulations (see Figure above).

This module contains two subroutines:

.. f:autosubroutine:: init_calendar

.. note::  init_calendar should be called in readfield if ints == intstart and also each time step.

.. f:autosubroutine:: update_calendar




