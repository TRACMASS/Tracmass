.. TRACMASS documentation master file, created by
   sphinx-quickstart on Mon Dec 23 22:14:01 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to TRACMASS's documentation!
====================================

TRACMASS is a Lagrangian trajectory code for ocean and atmospheric general circulation models. The code makes it possible to estimate water paths, Lagrangian stream functions (barotropic, overturning, etc.), exchange times, etc. TRACMASS has been used in studies of the global ocean circulation, of sea circulation in the Baltic Sea, the Mediterranean Sea and in coastal regions.

The code is written in FORTRAN 90 with modules and runs on UNIX platforms such as MAC OS X and Linux.

TRACMASS has been set up to run with velocities integrated with models such as NEMO or IFS-ECMWF, of satellite datasets such as AVISO.

Quickstart
==========

1. Download the code:

  .. code-block:: none

      git clone https://github.com/TRACMASS/Tracmass.git


2. Enter the TRACMASS directory

  .. code-block:: none

      cd Tracmass


3. Modify the *Makefile* to fit your system. You will need to set ARCH, which is the name of your system, i.e. tetralith. You will also need to configure how TRACMASS should find the netCDF libraries, if at all. For most systems, we recommend the option **automatic-44**.

  Make sure in *Makefile* both PROJECT and CASE are set to Theoretical. In this case, TRACMASS will use a simple oscillating velocity field to trace trajectories.

4. Then you can run the make command

  .. code-block:: none

      make

6. Run the code by typing

  .. code-block:: none

      ./runtracmass


.. toctree::
   :maxdepth: 2
   :caption: Contents:

   Introduction.rst
   Configuration.rst
   TRACMASS.rst
   Modules.rst
   Project.rst
   Output.rst
   Examples.rst
   References.rst

Indices and tables
==================

* :ref:`genindex`
