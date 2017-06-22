#include <Magnum/DefaultFramebuffer.h>
#include <Magnum/Platform/Sdl2Application.h>
#include <fstream>
#include <iostream>
using namespace Magnum;
class MyApplication : public Platform::Application {
public:
  explicit MyApplication(const Arguments &arguments);

private:
  void drawEvent() override;
};

//! @brief
//!
//! @usage
//!
//! @param arguments
//!
//! @return
MyApplication::MyApplication(const Arguments &arguments)
    : Platform::Application(arguments) {
  // add some code;
  ;
}
void MyApplication::drawEvent() {
  defaultFramebuffer.clear(FramebufferClear::Color);
  swapBuffers();
}
MAGNUM_APPLICATION_MAIN(MyApplication)