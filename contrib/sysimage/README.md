# CedarSim system image build recipe

This builds a system image of the CedarSim.jl package for quicker startups and loading times.
We will build upon this to create a container for use with JuliaHub for shipping CedarSim to clients.
In the future, we may want to generate a large number of precompile statements to really juice up the amount of code we're caching here.
Note that for the time being, I believe that we can only cache `NativeInterpreter` code, not `DAEInterpreter` code, so first circuit solves will still take a long time.

### Future work
- Assemble a representative workload, pass it as a precompile script.
- Add various interesting workloads (such as time-to-first-solve for the Lorenz problem) to the sysimage CI run.
- Bundle this system image into a container for deployment in JuliaHub.
- Enable DAEInterpreter code caching.
- Switch back to mainline PackageCompiler after https://github.com/JuliaLang/PackageCompiler.jl/pull/794 is merged.
