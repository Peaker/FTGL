FTGL_OBJS=                                           \
  ./src/FTSize.ftgl.o                              \
  ./src/FTLibrary.ftgl.o                           \
  ./src/FTCharmap.ftgl.o                           \
  ./src/FTGlyphContainer.ftgl.o                    \
  ./src/FTPoint.ftgl.o                             \
  ./src/FTFace.ftgl.o                              \
  ./src/FTFont/FTExtrudeFont.ftgl.o                \
  ./src/FTFont/FTPixmapFont.ftgl.o                 \
  ./src/FTFont/FTOutlineFont.ftgl.o                \
  ./src/FTFont/FTBitmapFont.ftgl.o                 \
  ./src/FTFont/FTPolygonFont.ftgl.o                \
  ./src/FTFont/FTBufferFont.ftgl.o                 \
  ./src/FTFont/FTFontGlue.ftgl.o                   \
  ./src/FTFont/FTTextureFont.ftgl.o                \
  ./src/FTFont/FTFont.ftgl.o                       \
  ./src/FTContour.ftgl.o                           \
  ./src/FTBuffer.ftgl.o                            \
  ./src/FTGlyph/FTGlyph.ftgl.o                     \
  ./src/FTGlyph/FTTextureGlyph.ftgl.o              \
  ./src/FTGlyph/FTPolygonGlyph.ftgl.o              \
  ./src/FTGlyph/FTGlyphGlue.ftgl.o                 \
  ./src/FTGlyph/FTPixmapGlyph.ftgl.o               \
  ./src/FTGlyph/FTExtrudeGlyph.ftgl.o              \
  ./src/FTGlyph/FTBufferGlyph.ftgl.o               \
  ./src/FTGlyph/FTOutlineGlyph.ftgl.o              \
  ./src/FTGlyph/FTBitmapGlyph.ftgl.o               \
  ./src/FTVectoriser.ftgl.o                        \
  ./src/FTLayout/FTLayout.ftgl.o                   \
  ./src/FTLayout/FTLayoutGlue.ftgl.o               \
  ./src/FTLayout/FTSimpleLayout.ftgl.o             \
  
FREETYPE_OBJS=\
  freetype2/src/base/ftsystem.freetype2.o               \
  freetype2/src/base/ftdebug.freetype2.o               \
  freetype2/src/base/ftinit.freetype2.o               \
  freetype2/src/base/ftbase.freetype2.o               \
  freetype2/src/base/ftbbox.freetype2.o               \
  freetype2/src/base/ftbdf.freetype2.o               \
  freetype2/src/base/ftbitmap.freetype2.o               \
  freetype2/src/base/ftcid.freetype2.o               \
  freetype2/src/base/ftfstype.freetype2.o               \
  freetype2/src/base/ftgasp.freetype2.o               \
  freetype2/src/base/ftglyph.freetype2.o               \
  freetype2/src/base/ftgxval.freetype2.o               \
  freetype2/src/base/ftlcdfil.freetype2.o               \
  freetype2/src/base/ftmm.freetype2.o               \
  freetype2/src/base/ftotval.freetype2.o               \
  freetype2/src/base/ftpatent.freetype2.o               \
  freetype2/src/base/ftpfr.freetype2.o               \
  freetype2/src/base/ftstroke.freetype2.o               \
  freetype2/src/base/ftsynth.freetype2.o               \
  freetype2/src/base/fttype1.freetype2.o               \
  freetype2/src/base/ftwinfnt.freetype2.o               \
  freetype2/src/base/ftxf86.freetype2.o               \
  freetype2/src/truetype/truetype.freetype2.o               \
  freetype2/src/type1/type1.freetype2.o               \
  freetype2/src/cff/cff.freetype2.o               \
  freetype2/src/cid/type1cid.freetype2.o               \
  freetype2/src/pfr/pfr.freetype2.o               \
  freetype2/src/type42/type42.freetype2.o               \
  freetype2/src/winfonts/winfnt.freetype2.o               \
  freetype2/src/pcf/pcf.freetype2.o               \
  freetype2/src/bdf/bdf.freetype2.o               \
  freetype2/src/sfnt/sfnt.freetype2.o               \
  freetype2/src/autofit/autofit.freetype2.o               \
  freetype2/src/pshinter/pshinter.freetype2.o               \
  freetype2/src/raster/raster.freetype2.o               \
  freetype2/src/smooth/smooth.freetype2.o               \
  freetype2/src/cache/ftcache.freetype2.o               \
  freetype2/src/gzip/ftgzip.freetype2.o               \
  freetype2/src/lzw/ftlzw.freetype2.o               \
  freetype2/src/psaux/psaux.freetype2.o               \
  freetype2/src/psnames/psmodule.freetype2.o

default: all
all: libftgl.dll

FREETYPE2_OPTS=\
  '-DFT_CONFIG_CONFIG_H=<ftconfig.h>' \
  '-DFT2_BUILD_LIBRARY' \
  '-DFT_CONFIG_MODULES_H=<ftmodule.h>' \
  '-DDARWIN_NO_CARBON' \

# Freetype:
%.freetype2.o: %.c
	gcc -o $@ -c $< -fPIC $(FREETYPE2_OPTS) -Ifreetype2/include -Ifreetype2/include/freetype/config

# FTGL
%.ftgl.o: %.cpp
	g++ -o $@ -c $< -fPIC -I. -Isrc -Ifreetype2/include

libfreetype.a: $(FREETYPE_OBJS)
	ar rc $@ $^

libftgl.dll: $(FTGL_OBJS) libfreetype.a
	g++ -o $@ $^ -shared -lglu32 -lopengl32
