#include <Magnum/Buffer.h>
#include <Magnum/Context.h>
#include <Magnum/DefaultFramebuffer.h>
#include <Magnum/Math/Color.h>
#include <Magnum/Mesh.h>
#include <Magnum/Platform/Sdl2Application.h>
#include <Magnum/Renderer.h>
#include <Magnum/Shaders/Shaders.h>
#include <Magnum/Shaders/VertexColor.h>
#include <Magnum/Version.h>
#include <array>
#include <fstream>
#include <iostream>
using namespace std;
using namespace Magnum;
using namespace Magnum::Math::Literals;
class MyApplication : public Platform::Application {
public:
  explicit MyApplication(const Arguments &arguments);

private:
  void drawEvent() override;
  Buffer m_buffer;
  Mesh m_mesh;
  Shaders::VertexColor2D m_shader;
};

struct TriangleVertex {
  Vector2 position;
  Color3 color;
};

array<TriangleVertex, 3> data({{{{(-5.e-1f), (-5.e-1f)}, 0xFF0000_srgbf},
                                {{(5.e-1f), (-5.e-1f)}, 0x00FF00_srgbf},
                                {{(0.0e+0f), (5.e-1f)}, 0x0000FF_srgbf}}});

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
  m_buffer.setData(data, BufferUsage::StaticDraw);
  m_mesh.setPrimitive(MeshPrimitive::Triangles)
      .setCount(3)
      .addVertexBuffer(m_buffer, 0, Shaders::VertexColor2D::Position{},
                       Shaders::VertexColor2D::Color{
                           Shaders::VertexColor2D::Color::Components::Three});
  (Debug() << "running on: " << Context::current().version() << " using "
           << Context::current().rendererString());
}
void MyApplication::drawEvent() {
  defaultFramebuffer.clear(FramebufferClear::Color);
  m_mesh.draw(m_shader);
  swapBuffers();
}
MAGNUM_APPLICATION_MAIN(MyApplication)