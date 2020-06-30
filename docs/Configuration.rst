Configuration
=============

The present chapter will present how to configure and run the TRACMASS Lagrangian trajectory model, as well as how to configure a customised experiment.

Installation
------------

1. Download the code

  .. code-block:: none

    git clone https://github.com/AitorAldama/Tracmass_light.git

2. Enter the TRACMASS directory

  .. code-block:: none

    cd Tracmass_light

3. Modify the *Makefile* to fit your system. You will need to set ARCH, which is the name of your system, i.e. tetralith. You will also need to configure how TRACMASS should find the netCDF libraries, if at all. For most systems, we recommend the option **automatic-44**.

4. Then you can run the make command

  .. code-block:: none

    make

How to run the test case
------------------------

We recommend testing that TRACMASS was properly compiled by letting PROJECT and CASE be **"Theoretical"** in the *Makefile* (which is the default). In this case, TRACMASS will use a simple oscillating velocity field to trace trajectories.

1. Make sure in *Makefile* both PROJECT and CASE are set to Theoretical.

2. Recompile the code

  .. code-block:: none

      make clean
      make

3. Run the code by typing

  .. code-block:: none

      ./runtracmass

4. If everything went without an error, you will get the following output in the terminal:

  .. code-block:: none

    ===============================================================================
                 TRACMASS Lagrangian off-line particle tracking
    ===============================================================================

    Start date  : 2020-06-30
    Start time  : 20:57:55
    -------------------------------------------------------------------------------

    Model information:
    Project code  : Theoretical
    Case name     : Theoretical
    Namelist file : projects/Theoretical/namelist_Theoretical.in
    -------------------------------------------------------------------------------

    Output information:
    Directory for output files : /Users/user/Tracmass_output
    Prefix for output files    : test
    -------------------------------------------------------------------------------

    Configuration options:
     - Two-dimensional trajectories, no change in depth
    -------------------------------------------------------------------------------

    Start date in model-time     : 2000-01-01 12:00
    End date in model-time       : 2000-01-01 21:00
    Length of run in timesteps   :     10
    Number of seeding timesteps  :      1
    Steps between two GCM fields :    100

    -------------------------------------------------------------------------------
    t-step        run        out        err        tot                 model date
    -------------------------------------------------------------------------------
         1 |        1 |        0 |        0 |        1 |   2000-01-01    12:00:00
         2 |        1 |        0 |        0 |        1 |   2000-01-01    13:00:00
         3 |        1 |        0 |        0 |        1 |   2000-01-01    14:00:00
         4 |        1 |        0 |        0 |        1 |   2000-01-01    15:00:00
         5 |        1 |        0 |        0 |        1 |   2000-01-01    16:00:00
         6 |        1 |        0 |        0 |        1 |   2000-01-01    17:00:00
         7 |        1 |        0 |        0 |        1 |   2000-01-01    18:00:00
         8 |        1 |        0 |        0 |        1 |   2000-01-01    19:00:00
         9 |        1 |        0 |        0 |        1 |   2000-01-01    20:00:00
        10 |        1 |        0 |        0 |        1 |   2000-01-01    21:00:00
    ===============================================================================
              1  particles calculated
              0  particles exited the space and time domain
              0  particles flagged with errors
              1  particles in domain
    -------------------------------------------------------------------------------

    ===============================================================================
    End date  : 2020-06-30
    End time  : 20:57:55
    ===============================================================================

5. You can rerun the code by modifying the *namelist.in* in the main directory.

How to configure a customised experiment
----------------------------------------

This is a basic guidance on how to implement a new setup from a GCM with a grid that does not exist in the present TRACMASS projects list. This requires good knowledge in the numerical discretisation of your GCM and of Fortran programming.

1. Make a new *<project>*, which we call "mymodel" here.

  Copy the directory from an existing project that has a similar grid (A,B or C-grid) or has a similar format (netcdf, grib, etc.). So that::

    cd projects
    cp -r NEMO mymodel

  or::

    cp -r AVISO mymodel

  or::

    cp -r IFS mymodel

2. Rename the namelist file to *namelist_mymodel.in* or *namelist_mycase.in* if you are using a case within the project.

3. Modify and adapt the files under */projects/mymodel*, which are *setup_grid.F90*, *read_field.F90*, and *kill_zones.F90*.

4. Adapt the *Makefile.prj* to adjust the compiling flags.

5. Adapt the *Makefile* in the main directory and set PROJECT = mymodel and CASE = mymodel.
