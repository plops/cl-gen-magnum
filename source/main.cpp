#include <Magnum/Context.h>
#include <Magnum/DefaultFramebuffer.h>
#include <Magnum/Math/Color.h>
#include <Magnum/Platform/Sdl2Application.h>
#include <Magnum/Renderer.h>
#include <Magnum/Version.h>
#include <fstream>
#include <iostream>
using namespace Magnum;
using namespace Magnum::Math::Literals;
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
  Renderer::setClearColor(Color3::fromHsv(216.0_degf, (8.5e-1f), (1.e+0f)));
  (Debug() << "running on: " << Context::current().version() << " using "
           << Context::current().rendererString());
}
void MyApplication::drawEvent() {
  defaultFramebuffer.clear(FramebufferClear::Color);
  swapBuffers();
}
MAGNUM_APPLICATION_MAIN(MyApplication)