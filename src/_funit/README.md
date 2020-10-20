Test suites for TRACMASS modules
================================

Different test suites have been written to several modules in TRACMASS. These test routines have been performed using fUnit and can be run locally:

1. Install fUnit:
```bash
sudo gem install funit
```

2. Go to the TRACMASS main directory and configure the runtest:
```bash
make test
```

3. Run the test:
```bash
./runtest.sh
```

The test_suites have been applied to the following modules in TRACMASS:

| :white_check_mark: Modules with test_suites | :x: Modules without test_suites |
|---------------------------------------------|---------------------------------|
| mod_calendar.F90                            | mod_init.F90                    |
| mod_clock.F90                               | mod_loop.F90                    |
| mod_error.F90                               | mod_postprocess.F90             |
| mod_getfile.F90                             | mod_print.F90                   |
| mod_pos.F90                                 | mod_stream.F90                  |
| mod_seed.F90                                | mod_vars.F90                    |
| mod_subdomain.F90                           | mod_write.F90                   |
| mod_swap.F90                                |                                 |
| mod_tracers.F90 / mod_tracerf.F90           |                                 |
| mod_vertvel.F90                             |                                 |
