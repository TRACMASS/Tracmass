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

This module contains three subroutines:

.. f:autosubroutine:: init_calendar

.. note::  init_calendar should be called in readfield if ints == intstart and also each time step.

.. f:autosubroutine:: update_calendar

.. f:autosubroutine:: end_calendar

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

mod_pos
--------

The module **mod_pos** calculates the new position of a trajectory and the time it will take to cross a wall in the gridbox. This module contains three subroutines: **cross_time**, **calc_pos**, and **update_traj**.

* The subroutine **cross_time** computes the time it will take to cross any of the faces determined by the variable **ijk**. The possible values of **ijk** are (1) for the east/west faces, (2) for the north/south faces, and (3) for the up/down faces. For simplicity, let us consider the case of **ijk** =1 with an eastward zonal flow **uflux**.

  .. image:: figs/fig_boxpos.png
      :width: 300px
      :align: center
      :height: 300px
      :alt: Description of mod_pos

  This is how the module works:

  1 - First, the interpolated values of the zonal flow **uflux** in the east wall (**uu**) and in the west wall (**um**) are computed. A linear interpolation is used to obtained the values for **uu** and **um**:

  .. math::

     U_i(t) = \frac{(t-t_0) \ U_i(t_1) + (t_1-t) \ U_i(t_0)}{t_1-t_0} \quad \text{where} \quad t_0 \le t \le t_1.

  In this case :math:`t_0` and :math:`t_1` represent the *n* and *n+1* time step.

  2 - If **uu** is positive the subroutine computes the crossing time through the eastern wall :math:`t_E` (**sp**).

  2.1 - If  **uu** = **um**, the time it will take to the trajectory to cross the east wall is:

  .. math::

     t_{E} = \frac{x_E-x}{U_i(t)}.

  2.2 - If **uu** :math:`\neq` **um**, the velocity field inside the box is linearly interpolated:

  .. math::

     U(x) = (x-x_W)(U(x_E)-U(x_W)) + U(x_W) \quad \text{where} \quad x_W \le x \le x_E,

  where **iam** represents the index for the western wall (:math:`x_W`) and **ia** represents the eastern wall (:math:`x_E`). If :math:`U(x)>0` at the starting position of the particle, the time to reach the eastern wall is given by:

  .. math::

     t_{E} = \frac{1}{U(x_W)-U(x_E)}\log\left(\frac{U(x)}{U_E} \right).

  2.3 - If none of the above conditions is fulfilled the subroutine returns the value **UNDEF** for **sp**.

  3 - Following a similar procedure, the subroutine computes the crossing time through the western wall (**sn**).

.. note:: In the computation **sn** the equations used to compute the crossing time considers a different spatial interpolation of :math:`U(x)`. The crossing time through the western wall is given by the following equation :math:`t_{W} = \frac{1}{U(x_W)-U(x_E)}\log\left(\frac{U(x)}{U_W} \right)`.

* The subroutine **calc_pos** computes the new position of the trajectory after time **ds** in the direction given by **ijk**. This subroutine works in the following way (let us consider the same case as in the previous example for **cross_time**):

  1 - First, the interpolated values of the zonal flow **uflux** in the east wall (**uu**) and in the west wall (**um**) are computed. A linear interpolation is used to obtained the values for **uu** and **um**.

  2.1 - If  **uu** = **um**, the new position of the trajectory is given by:

  .. math::

     x_1 = x_0 + U(x_E)ds

  2.2 - On the other hand, if **uu** :math:`\neq` **um** the new position is:

  .. math::
     x_1 = \left(x_0 - x_W + \frac{U(x_W)}{U(x_E)-U(x_W)} \right) \exp((U(x_E)-U(x_W))ds) + x_W - \frac{U(x_W)}{U(x_E)-U(x_W)}.

.. warning:: If the trajectory is placed at :math:`U(x)=0` in a divergent field, **calc_pos** is not able to determine the new position (unstable equilibrium).

* The subroutine **update_traj** updates the position of the trajectory after a time step given by **ds** and computes the new values for **x1**, **y1**, and **z1**. The subroutines check if any of the crossing values given by **cross_time** corresponds to the value of **ds** to determine the new position.

                        +---------+----------+---------+--------+----------------+
                        | **ds**  |  **ib**  | **jb**  | **kb** | Crossing wall  |
                        +=========+==========+=========+========+================+
                        |   dse   |  ia + 1  |         |        | Eastern wall   |
                        +---------+----------+---------+--------+----------------+
                        |   dsw   |  ia - 1  |         |        | Western wall   |
                        +---------+----------+---------+--------+----------------+
                        |   dsn   |          |  ja + 1 |        | Northern wall  |
                        +---------+----------+---------+--------+----------------+
                        |   dss   |          |  ja - 1 |        | Northern wall  |
                        +---------+----------+---------+--------+----------------+
                        |   dsu   |          |         | ka + 1 | Upper wall     |
                        +---------+----------+---------+--------+----------------+
                        |   dsd   |          |         | ka - 1 | Lower wall     |
                        +---------+----------+---------+--------+----------------+

If **ds** is smaller than any of the crossing times and equal to the time stepping, or if the trajectory is inside a convergence zone where all the crossing times are **UNDEF**. The trajectory remains inside the box.

This module contains three subroutines:

.. f:autosubroutine:: cross_time

.. f:autosubroutine:: calc_pos

.. f:autosubroutine:: update_traj


mod_time
--------

The module **mod_time** calculates the new time step referenced to the initial time step. This module contains one subroutine **update_time**.

.. image:: figs/fig_time.png
    :width: 500px
    :align: center
    :height: 225px
    :alt: Description of mod_time

The subroutine updates **tt** and **ts** based on the value of **ds**. This is transform to a time step in seconds **dt** by multiplying **ds** with the volume **dxyz**. The subroutine chooses between the smallest of three different time steps:

1 - **dtmin** which is the time step between two time subcycles :math:`t_{min} = \frac{\Delta t}{iter}` where **iter** is the number of subcycles.

2 - **dtreg** which is the time step to the next time subcycle.

3 - And the time step corresponding to the smallest wall crossing time computed with **cross_time**.

After updating the values of **tt** and **ts**, the new values of **intrpb** and **intrpr** are computed.

.. f:autosubroutine:: update_time
