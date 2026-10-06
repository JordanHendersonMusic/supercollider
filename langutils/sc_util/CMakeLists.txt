add_library(sc_util INTERFACE 
	include/strong_index.hpp
	include/type_set_index.hpp
	include/typed_graph_node.hpp
	include/typed_graph.hpp
)

target_include_directories(sc_util INTERFACE include)
set_property(TARGET sc_util PROPERTY FOLDER Libs)
