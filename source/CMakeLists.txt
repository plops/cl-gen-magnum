cmake_minimum_required(VERSION 3.5)
set(CMAKE_MODULE_PATH /home/martin/src/magnum/modules)
#find_package(SDL2 REQUIRED)

find_package(Magnum REQUIRED Shaders Primitives MeshTools Sdl2Application)
set_directory_properties(PROPERTIES CORRADE_USE_PEDANTIC_FLAGS ON)
add_executable(MyApplication main.cpp)
target_link_libraries(MyApplication
  Magnum::Magnum
  Magnum::MeshTools
  Magnum::Primitives
    Magnum::Application
    Magnum::Shaders)
