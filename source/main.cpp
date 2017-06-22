#include <Magnum/Buffer.h>
#include <Magnum/Context.h>
#include <Magnum/DefaultFramebuffer.h>
#include <Magnum/Math/Color.h>
#include <Magnum/Mesh.h>
#include <Magnum/MeshTools/CompressIndices.h>
#include <Magnum/MeshTools/Interleave.h>
#include <Magnum/Platform/Sdl2Application.h>
#include <Magnum/Primitives/Cube.h>
#include <Magnum/Renderer.h>
#include <Magnum/Shaders/Phong.h>
#include <Magnum/Shaders/Shaders.h>
#include <Magnum/Trade/MeshData3D.h>
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
  void mousePressEvent(MouseEvent &event) override;
  void mouseReleaseEvent(MouseEvent &event) override;
  void mouseMoveEvent(MouseMoveEvent &event) override;
  Buffer m_index_buffer;
  Buffer m_vertex_buffer;
  Mesh m_mesh;
  Shaders::Phong m_shader;
  Matrix4 m_transformation;
  Matrix4 m_projection;
  Vector2i m_previousMousePosition;
  Color3 m_color;
};

struct TriangleVertex {
  Vector2 position;
  Color3 color;
};

array<TriangleVertex, 3> data({{{{(-5.e-1f), (-5.e-1f)}, 0xFF0000_srgbf},
                                {{(5.e-1f), (-5.e-1f)}, 0x00FF00_srgbf},
                                {{(0.0e+0f), (5.e-1f)}, 0x0000FF_srgbf}}});

MyApplication::MyApplication(const Arguments &arguments)
    : Platform::Application(arguments) {
  Renderer::setClearColor(Color3::fromHsv(216.0_degf, (8.5e-1f), (1.e+0f)));
  Renderer::enable(Renderer::Feature::DepthTest);
  Renderer::enable(Renderer::Feature::FaceCulling);
  {
    Trade::MeshData3D cube(Primitives::Cube::solid());
    m_vertex_buffer.setData(
        MeshTools::interleave(cube.positions(0), cube.normals(0)),
        BufferUsage::StaticDraw);
    {
      Containers::Array<char> index_data;
      Mesh::IndexType index_type;
      UnsignedInt index_start;
      UnsignedInt index_end;
      tie(index_data, index_type, index_start, index_end) =
          MeshTools::compressIndices(cube.indices());
      m_index_buffer.setData(index_data, BufferUsage::StaticDraw);
      m_mesh.setPrimitive(cube.primitive());
      m_mesh.setCount(cube.indices().size());
      m_mesh.addVertexBuffer(m_vertex_buffer, 0, Shaders::Phong::Position{},
                             Shaders::Phong::Normal{});
      m_mesh.setIndexBuffer(m_index_buffer, 0, index_type, index_start,
                            index_end);
    }
  }
  m_transformation =
      (Matrix4::rotationX(30.0_degf) * Matrix4::rotationY(40.0_degf));
  m_color = Color3::fromHsv(35.0_degf, (1.e+0f), (1.e+0f));
  m_projection =
      (Matrix4::perspectiveProjection(
           35.0_degf,
           Vector2{defaultFramebuffer.viewport().size()}.aspectRatio(),
           (1.e-2f), (1.e+2f)) *
       Matrix4::translation(Vector3::zAxis((-1.e+1f))));
  (Debug() << "running on: " << Context::current().version() << " using "
           << Context::current().rendererString());
}
void MyApplication::drawEvent() {
  defaultFramebuffer.clear((FramebufferClear::Color | FramebufferClear::Depth));
  m_shader.setLightPosition({(7.e+0f), (5.e+0f), (2.5e+0f)});
  m_shader.setLightColor(Color3{1.0f});
  m_shader.setDiffuseColor(m_color);
  m_shader.setAmbientColor(Color3::fromHsv(m_color.hue(), (1.e+0f), (3.e-1f)));
  m_shader.setTransformationMatrix(m_transformation);
  m_shader.setNormalMatrix(m_transformation.rotationScaling());
  m_shader.setProjectionMatrix(m_projection);
  m_mesh.draw(m_shader);
  swapBuffers();
}
MAGNUM_APPLICATION_MAIN(MyApplication)