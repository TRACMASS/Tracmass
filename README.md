TRACMASS v2022.01
=====================
[![GitHubAction](https://github.com/AitorAldama/Tracmass/workflows/Build%20and%20Test/badge.svg)](https://github.com/AitorAldama/Tracmass/workflows/Build%20and%20Test/badge.svg)  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4337926.svg)](https://doi.org/10.5281/zenodo.4337926)

![Social logo](https://github.com/TRACMASS/tracmass.github.io/blob/version7/images/headers_footer/fig_socialimage.png)

**TRACMASS** is a Lagrangian trajectory code for ocean and atmospheric general circulation models. The code makes it possible to estimate water paths, Lagrangian stream functions (barotropic, overturning, etc.), exchange times, etc. **TRACMASS** has been used in studies of the global ocean circulation, of sea circulation in the Baltic Sea, the Mediterranean Sea and in coastal regions.

The code is written in FORTRAN 90 with modules and runs on UNIX platforms such as MAC OS X and Linux.

**TRACMASS** has been set up to run with velocities integrated with models such as NEMO or IFS-ECMWF, of satellite datasets such as AVISO.

For more information check our website: **https://www.tracmass.org/**.

Documentation
=============

You can find documentation about TRACMASS [**here**](https://www.tracmass.org/docs)

Quickstart
==========

### 1. Download TRACMASS

```bash
git clone https://github.com/TRACMASS/Tracmass.git
```

### 2. Enter the TRACMASS directory

```bash
cd Tracmass
```

### 3. Modify the *Makefile* to fit your system.

You will need to set `ARCH` - name of your system, (i.e. tetralith) - which is used to set the Fortran compiler and flags.
    
You also need to configure how **TRACMASS** should find the netCDF libraries, if at all, using `NETCDFLIBS`.

For most HPC systems, we recommend setting netCDF libraries using either the `NETCDFLIBS = automatic-44` option for netCDF version >4.4 or `NETCDFLIBS = automatic` otherwise.

---
#### **3.1 Using Conda:**

The simplest way to get started with **TRACMASS** on a local machine or HPC system is to use a conda virtual environment.

To do this:

* Create a new virtual environment using [**miniforge**](https://github.com/conda-forge/miniforge) - a minimal installer for **Conda** and **Mamba**:
    ```bash
    conda create -n env_tracmass
    ```

* Next, install the **gfortran** and **netcdf-fortran** libraries required by **TRACMASS** from the conda-forge open-source package manager.
    ```bash
    conda activate env_tracmass

    conda install gfortran netcdf-fortran
    ```

* Verify **gfortran** and **netCDF** libraries are available.
    ```bash
    which gfortran

    nf-config --all
    ```

* Update the *Makefile* `ARCH` and `NETCDFLIBS` options.
    ```bash
    # Project and case definition
    ...
    ARCH              = conda
    NETCDFLIBS        = conda
    #=============================
    ```

---

### 4. Compile TRACMASS

```bash
make
```

Running the First Test Case
---------------------------

We recommend testing that **TRACMASS** was properly compiled by letting `PROJECT` and `CASE` be **"Theoretical"** in the *Makefile* (which is the default).

In this case, **TRACMASS** will use a simple oscillating velocity field to trace trajectories.

1. Make sure in *Makefile* both `PROJECT` and `CASE` are set to `Theoretical`.

    ```bash
    # Project and case definition
    PROJECT	          = Theoretical
    CASE              = Theoretical
    ...
    ```

2. Recompile **TRACMASS**

    ```bash
    make clean
    make
    ```

3. Run **TRACMASS**

    ```bash
    ./runtracmass
    ```

Download Example Data
---------------------

You can find example input data for testing the code here: **https://stockholmuniversity.box.com/s/pyc29hjumxuvf0nf84ym5zs0vrnqbswr**

This includes data from NEMO, IFS and AVISO. Before doing any analysis we recommend to download some of the test data and make sure **TRACMASS** is working properly.

For example, in order to set up **TRACMASS** to run trajectories using NEMO model output data, you will need to change `PROJECT` and `CASE` to `NEMO`, and then re-compile the code.

```bash
make clean
make

./runtracmass
```

Perform Your Own Analysis
-------------------------

If you wish to run another case or a very specific case of the above models, you will need create your own project in the `projects` directory.

For example, to run with your own IFS data, you will need to modify the `namelist_IFS.in` namelist in the `projects/IFS/` directory to suit your needs.

Previous Versions of TRACMASS
=============================

Previous versions of **TRACMASS** are stored in the following repository:

**https://github.com/TRACMASS/Tracmass_previous.git**

The following features from older versions are not available yet in this version:

* Subgrid turbulence parameterisations
