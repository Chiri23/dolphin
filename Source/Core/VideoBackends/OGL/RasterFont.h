// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <string>

namespace OGL
{

class RasterFont
{
public:
	RasterFont();
	~RasterFont(void);
	static int debug;

	void printMultilineText(const std::string& text, double x, double y, double z, int bbWidth, int bbHeight, u32 color);
private:

	u32 VBO;
	u32 VAO;
	u32 texture;
	u32 uniform_color_id;
	u32 cached_color;
};

}
