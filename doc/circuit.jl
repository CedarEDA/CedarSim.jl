using PythonCall
schemdraw = pyimport("schemdraw")
elm = schemdraw.elements
schemdraw.use("svg")
schemdraw.backends.svg.inline = true
(<|)(c::Py, e::Py) = c.__iadd__(e)