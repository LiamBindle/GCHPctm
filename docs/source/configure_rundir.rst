

Configure your run directory
============================

Navigate to your new run directory, and set it up for the first run:

.. code-block:: console

   $ cd /scratch/testruns/GCHP/13.0.0/fullchem_first_test
   $ ./setEnvironment /home/envs/gchpctm_ifort18.0.5_openmpi4.0.1.env # This sets up the gchp.env symlink
   $ source gchp.env # Set up build environment, if not already done
   $ cp runScriptSamples/gchp.run . # Set up run script - your system is likely to be different! See also gchp.local.run.
   $ cp CodeDir/build/bin/geos . # Get the compiled executable

