OBJS=                                           \
  ./src/FTSize.o                              \
  ./src/FTLibrary.o                           \
  ./src/FTCharmap.o                           \
  ./src/FTGlyphContainer.o                    \
  ./src/FTPoint.o                             \
  ./src/FTFace.o                              \
  ./src/FTFont/FTExtrudeFont.o                \
  ./src/FTFont/FTPixmapFont.o                 \
  ./src/FTFont/FTOutlineFont.o                \
  ./src/FTFont/FTBitmapFont.o                 \
  ./src/FTFont/FTPolygonFont.o                \
  ./src/FTFont/FTBufferFont.o                 \
  ./src/FTFont/FTFontGlue.o                   \
  ./src/FTFont/FTTextureFont.o                \
  ./src/FTFont/FTFont.o                       \
  ./src/FTContour.o                           \
  ./src/FTBuffer.o                            \
  ./src/FTGlyph/FTGlyph.o                     \
  ./src/FTGlyph/FTTextureGlyph.o              \
  ./src/FTGlyph/FTPolygonGlyph.o              \
  ./src/FTGlyph/FTGlyphGlue.o                 \
  ./src/FTGlyph/FTPixmapGlyph.o               \
  ./src/FTGlyph/FTExtrudeGlyph.o              \
  ./src/FTGlyph/FTBufferGlyph.o               \
  ./src/FTGlyph/FTOutlineGlyph.o              \
  ./src/FTGlyph/FTBitmapGlyph.o               \
  ./src/FTVectoriser.o                        \
  ./src/FTLayout/FTLayout.o                   \
  ./src/FTLayout/FTLayoutGlue.o               \
  ./src/FTLayout/FTSimpleLayout.o

default: all
all: libftgl.so

%.o: %.cpp
	g++ -o $@ -c $< -fPIC -I. -Isrc -Ifreetype2 -Ifreetype2

libftgl.so: $(OBJS)
	g++ -o $@ $^ -shared -lstdc++
