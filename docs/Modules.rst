Modules
=======
.. _md_program:

mod_calendar
------------

The module **mod_calendar** corrects the calendar of the simulation using the given time step and the initial date. The calendar is computed for both forward and backward simulations in time.

Let us consider a simulation forward in time, where the start year (**StartYear**) is set to 2000 and the current year (**currYear**) is 2004. The variable **iyear** describes the number of simulation years. If **loopYears** is activated, after reaching the year set as **loopEndYear** (2010 in this case) the calendar is set back to the **loopStartYear** (2000). **iyear** is not altered by this correction.

.. image:: figs/fig_calendar.png
    :width: 700px
    :align: center
    :height: 600px
    :alt: Example of a forward and backward calendar
.. warning::  Note than in backward simulations in time, **loopStartYear>loopEndYear**. The start year (**StartYear**) is always larger or equal to the current year (**currYear**) - unless **loopYears** is activated.  If **loopYears** is activated, the calendar is corrected in similar way to the forward simulations (see Figure above).

This module contains two subroutines:

.. f:autosubroutine:: init_calendar

.. note::  init_calendar should be called in readfield if ints == intstart and also each time step.

.. f:autosubroutine:: update_calendar

mod_seed
--------

The module **mod_seed** defines all the variables and arrays neccesary for the seeding of particles. This modules contains two public subroutines (**init_seed** and **seed**) and a private subroutine (split_grid)

The subroutine **init_seed** defines the grid points and the time steps where the particles are going to be initialised, the wall of the grid where they are going to be placed (**isec**), and their direction (**idir**). There are three options for **isec**: (1) on the east wall of the grid cell, (2) on the north wall of the grid cell, and (3) on the top wall of the grid cell. idir selects the initial direction of the trajectories eastward/northward/upward (**idir = 1**) or westward/southward/downward (**idir = -1**).

.. image:: figs/fig_isec.png
    :width: 389px
    :align: center
    :height: 300px
    :alt: Description of isec on the grid cell

.. note:: If the simulation is backward in time (**nff = -1**), idir represents the last direction of the trajectory to follow. For example, let us consider a eastward flow field. A simulation with **nff = -1** and **idir = 1** will follow trajectories back in time that initially are moving eastward.

The initial seeding location, time, and direction can be defined directly in the namelist or read from a file. This is control by **seedType** and **seedTime**.

* **seedType**: (1) the seeding location is defined by the grid points within the volume described by **(ist2-ist1+1)x(jst2-jst1+1)x(kst2-kst1+1)**, all these trajectories will shared the **idir** and **isec** defined in the namelist, or (2) the seeding location and the direction is read from an external file **seedDir/seedfile**.

* **seedTime**: (1) the seeding happens in the time interval defined between **tst2** and **tst1**, or (2) it is read from a external file **seedDir/timeFile**.


The **seed** subroutine populates the **trajectory** array that contains the position of the trajectories as well as their corresponding volume/mass transport. This module works this way:

1 - The subroutine checks if the current time **ntime** corresponds to a seeding time.

2 - **num**, the number of trajectories per grid point, is defined. There are different options based on **nqua**: (1) the number of trajectories is defined by **partQuant**, or (2) the particles transport a specific volume/mass transport defined by **partQuant**, the number of particles in the grid is then defined dividing the total volume/mass transport by **partQuant**.

.. image:: figs/fig_nqua.png
    :width: 600px
    :align: center
    :height: 450px
    :alt: Description of nqua

3 - The grid is split in equal parts using the private subroutine **split_grid**. If **num** is a square number the grid cell is divided in equal squares, if **num** is a prime number the grid is split in equal rectangles along one axis (see figure below). For other cases, **split_grid** will divide the square in equal rectangles with similar side lengths.

.. image:: figs/fig_num.png
    :width: 500px
    :align: center
    :height: 200px
    :alt: Description of nqua

4 - The specific volume/mass transport of a trajectory **subvol** is computed from **num**.

5 - The trajectories are placed in the middle of each of the rectangles. This initial position is given by **x1, y1, z1**.

.. warning:: **x1, y1, z1** are computed using the gridbox as a reference.

6 - The position of the trajectory in the gridbox reference system, the trajectory number **ntrac**, the corresponding position index and the mass/volume transported by it is stored in the array **trajectories**.

This module contains two public subroutines:

.. f:autosubroutine:: init_seed

.. f:autosubroutine:: seed

and a private subroutine:

.. f:autosubroutine:: split_grid
