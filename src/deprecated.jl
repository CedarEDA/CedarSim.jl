import DAECompiler: IRODESystem
Base.@deprecate IRODESystem(ac::ACSol) get_sys(ac)
Base.@deprecate IRODESystem(ac::NoiseSol)  get_sys(ac)
Base.@deprecate CircuitIRODESystem(sol::SciMLBase.AbstractODESolution)  get_sys(sol)