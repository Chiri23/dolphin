
#include "Common.h"

#include "Statistics.h"
#include "OpcodeDecoding.h"
#include "IndexGenerator.h"
#include "VertexShaderManager.h"
#include "PixelShaderManager.h"
#include "NativeVertexFormat.h"
#include "TextureCacheBase.h"
#include "RenderBase.h"
#include "BPStructs.h"
#include "XFMemory.h"
#include "Debugger.h"
#include "PerfQueryBase.h"

#include "VertexManagerBase.h"
#include "MainBase.h"
#include "VideoConfig.h"

// :chiri: nvapi
#include "../../../nvapi.h"
namespace DX11
{
	void *GetStereoHandle();
}
// :chiri: gameid
#include "ConfigManager.h"
u32 VertexManager::gameId;
// :chiri: debugging
#include <set>
#include <algorithm>
#include <Xinput.h>
#include "OnScreenDisplay.h"
extern "C" DWORD(__cdecl *PXInputGetState)(_In_  DWORD dwUserIndex, _Out_ XINPUT_STATE* pState);

VertexManager *g_vertex_manager;

u8 *VertexManager::s_pCurBufferPointer;
u8 *VertexManager::s_pBaseBufferPointer;
u8 *VertexManager::s_pEndBufferPointer;

PrimitiveType VertexManager::current_primitive_type;

bool VertexManager::IsFlushed;

static const PrimitiveType primitive_from_gx[8] = {
	PRIMITIVE_TRIANGLES, // GX_DRAW_QUADS
	PRIMITIVE_TRIANGLES, // GX_DRAW_NONE
	PRIMITIVE_TRIANGLES, // GX_DRAW_TRIANGLES
	PRIMITIVE_TRIANGLES, // GX_DRAW_TRIANGLE_STRIP
	PRIMITIVE_TRIANGLES, // GX_DRAW_TRIANGLE_FAN
	PRIMITIVE_LINES,     // GX_DRAW_LINES
	PRIMITIVE_LINES,     // GX_DRAW_LINE_STRIP
	PRIMITIVE_POINTS,    // GX_DRAW_POINTS
};

VertexManager::VertexManager()
{
	IsFlushed = true;
}

VertexManager::~VertexManager()
{
}

u32 VertexManager::GetRemainingSize()
{
	return (u32)(s_pEndBufferPointer - s_pCurBufferPointer);
}

void VertexManager::PrepareForAdditionalData(int primitive, u32 count, u32 stride)
{
	u32 const needed_vertex_bytes = count * stride;

	// We can't merge different kinds of primitives, so we have to flush here
	if (current_primitive_type != primitive_from_gx[primitive])
		Flush();
	current_primitive_type = primitive_from_gx[primitive];

	// Check for size in buffer, if the buffer gets full, call Flush()
	if ( !IsFlushed && ( count > IndexGenerator::GetRemainingIndices() ||
	     count > GetRemainingIndices(primitive) || needed_vertex_bytes > GetRemainingSize() ) )
	{
		Flush();

		if(count > IndexGenerator::GetRemainingIndices())
			ERROR_LOG(VIDEO, "Too little remaining index values. Use 32-bit or reset them on flush.");
		if (count > GetRemainingIndices(primitive))
			ERROR_LOG(VIDEO, "VertexManager: Buffer not large enough for all indices! "
				"Increase MAXIBUFFERSIZE or we need primitive breaking after all.");
		if (needed_vertex_bytes > GetRemainingSize())
			ERROR_LOG(VIDEO, "VertexManager: Buffer not large enough for all vertices! "
				"Increase MAXVBUFFERSIZE or we need primitive breaking after all.");
	}

	// need to alloc new buffer
	if(IsFlushed)
	{
		g_vertex_manager->ResetBuffer(stride);
		IsFlushed = false;
	}
}

u32 VertexManager::GetRemainingIndices(int primitive)
{
	u32 index_len = MAXIBUFFERSIZE - IndexGenerator::GetIndexLen();

	if(g_Config.backend_info.bSupportsPrimitiveRestart)
	{
		switch (primitive)
		{
		case GX_DRAW_QUADS:
			return index_len / 5 * 4;
		case GX_DRAW_TRIANGLES:
			return index_len / 4 * 3;
		case GX_DRAW_TRIANGLE_STRIP:
			return index_len / 1 - 1;
		case GX_DRAW_TRIANGLE_FAN:
			return index_len / 6 * 4 + 1;

		case GX_DRAW_LINES:
			return index_len;
		case GX_DRAW_LINE_STRIP:
			return index_len / 2 + 1;

		case GX_DRAW_POINTS:
			return index_len;

		default:
			return 0;
		}
	}
	else
	{
		switch (primitive)
		{
		case GX_DRAW_QUADS:
			return index_len / 6 * 4;
		case GX_DRAW_TRIANGLES:
			return index_len;
		case GX_DRAW_TRIANGLE_STRIP:
			return index_len / 3 + 2;
		case GX_DRAW_TRIANGLE_FAN:
			return index_len / 3 + 2;

		case GX_DRAW_LINES:
			return index_len;
		case GX_DRAW_LINE_STRIP:
			return index_len / 2 + 1;

		case GX_DRAW_POINTS:
			return index_len;

		default:
			return 0;
		}
	}
}

// :chiri: stereo debug hacks
static std::map<u64, DWORD> visitedHash;
static int selectedPos = 0;
static u64 selectedHash = 0;
static WORD keyState;
static SHORT tuneLX = 0, tuneLY = 0, tuneRX = 0, tuneRY = 0;
static bool checkHash(TextureCache::TCacheEntryBase *tentry, int stage)
{
	DWORD tickcnt = GetTickCount();
	u64 hash = tentry->hash;
	if (tentry->IsEfbCopy())
	{
		hash = tentry->format;
		hash = hash * 997 + tentry->native_width;
		hash = hash * 997 + tentry->native_height;
		hash = hash * 997 + stage;
	}
	visitedHash[hash] = tickcnt;
	for (std::map<u64, DWORD>::iterator i = visitedHash.begin(); i != visitedHash.end();)
	{
		if (tickcnt - i->second > 10000)
			i = visitedHash.erase(i);
		else
			++i;
	}

	XINPUT_STATE state;
	ZeroMemory(&state, sizeof(XINPUT_STATE));
	DWORD dwResult = (*PXInputGetState)(1, &state);
	if (dwResult == ERROR_SUCCESS)
	{
		tuneLX = state.Gamepad.sThumbLX;
		tuneLY = state.Gamepad.sThumbLY;
		tuneRX = state.Gamepad.sThumbRX;
		tuneRY = state.Gamepad.sThumbRY;
		if (abs(tuneLX) < 6000) tuneLX = 0;
		if (abs(tuneLY) < 6000) tuneLY = 0;
		if (abs(tuneRX) < 6000) tuneRX = 0;
		if (abs(tuneRY) < 6000) tuneRY = 0;
		if (state.Gamepad.wButtons != keyState)
		{
			keyState = state.Gamepad.wButtons;
			if (state.Gamepad.wButtons & XINPUT_GAMEPAD_RIGHT_SHOULDER)
			{
				selectedPos++;
				if (selectedPos >= visitedHash.size()) selectedPos = 0;				
				char info[256];
				sprintf(info, "Selected #%d", selectedPos);
				OSD::AddMessage(info, 1000);
			}
			if (state.Gamepad.wButtons & XINPUT_GAMEPAD_LEFT_SHOULDER)
			{
				selectedPos--;
				if (selectedPos < 0) selectedPos = visitedHash.size() - 1;
				char info[128];
				sprintf(info, "Selected #%d", selectedPos);
				OSD::AddMessage(info, 1000);
			}
			if (state.Gamepad.wButtons & XINPUT_GAMEPAD_A)
			{
				float *pstart = (float*)&xfmem[0];
				char info[128];
				sprintf(info, "Texture usage: stage = %d, width = %d, height = %d, hash = %08lx%08lx", stage,
					tentry->native_width, tentry->native_height, (UINT32)(selectedHash >> 32), (UINT32)selectedHash);
				OSD::AddMessage(info, 10000);
				sprintf(info, "               0=%f\t1=%f\t2=%f\t3=%f", pstart[0], pstart[1], pstart[2], pstart[3]);
				OSD::AddMessage(info, 10000);
				sprintf(info, "               4=%f\t5=%f\t6=%f\t7=%f", pstart[4], pstart[5], pstart[6], pstart[7]);
				OSD::AddMessage(info, 10000);
				sprintf(info, "               8=%f\t9=%f\t10=%f\t11=%f", pstart[8], pstart[9], pstart[10], pstart[11]);
				OSD::AddMessage(info, 10000);
			}
		}
		std::map<u64, DWORD>::iterator i = visitedHash.begin();
		std::advance(i, selectedPos);
		selectedHash = i->first;
	}
	bool found = hash == selectedHash;
	if (found && (tuneLX || tuneLY || tuneRX || tuneRY))
	{
		float *pstart = (float*)&xfmem[0];
		pstart[8] *= 1.0f + float(tuneLX) / 32768.f;
		pstart[9] *= 1.0f + float(tuneLY) / 32768.f;
		pstart[10] *= 1.0f + float(tuneRX) / 32768.f;
		pstart[11] *= 1.0f + float(tuneRY) / 32768.f;
		return false;
	}
	return found;
}


void VertexManager::Flush()
{
	if (IsFlushed) return;

	// loading a state will invalidate BP, so check for it
	g_video_backend->CheckInvalidState();

	VideoFifo_CheckEFBAccess();

#if defined(_DEBUG) || defined(DEBUGFAST)
	PRIM_LOG("frame%d:\n texgen=%d, numchan=%d, dualtex=%d, ztex=%d, cole=%d, alpe=%d, ze=%d", g_ActiveConfig.iSaveTargetId, xfregs.numTexGen.numTexGens,
		xfregs.numChan.numColorChans, xfregs.dualTexTrans.enabled, bpmem.ztex2.op,
		bpmem.blendmode.colorupdate, bpmem.blendmode.alphaupdate, bpmem.zmode.updateenable);

	for (unsigned int i = 0; i < xfregs.numChan.numColorChans; ++i)
	{
		LitChannel* ch = &xfregs.color[i];
		PRIM_LOG("colchan%d: matsrc=%d, light=0x%x, ambsrc=%d, diffunc=%d, attfunc=%d", i, ch->matsource, ch->GetFullLightMask(), ch->ambsource, ch->diffusefunc, ch->attnfunc);
		ch = &xfregs.alpha[i];
		PRIM_LOG("alpchan%d: matsrc=%d, light=0x%x, ambsrc=%d, diffunc=%d, attfunc=%d", i, ch->matsource, ch->GetFullLightMask(), ch->ambsource, ch->diffusefunc, ch->attnfunc);
	}

	for (unsigned int i = 0; i < xfregs.numTexGen.numTexGens; ++i)
	{
		TexMtxInfo tinfo = xfregs.texMtxInfo[i];
		if (tinfo.texgentype != XF_TEXGEN_EMBOSS_MAP) tinfo.hex &= 0x7ff;
		if (tinfo.texgentype != XF_TEXGEN_REGULAR) tinfo.projection = 0;

		PRIM_LOG("txgen%d: proj=%d, input=%d, gentype=%d, srcrow=%d, embsrc=%d, emblght=%d, postmtx=%d, postnorm=%d",
			i, tinfo.projection, tinfo.inputform, tinfo.texgentype, tinfo.sourcerow, tinfo.embosssourceshift, tinfo.embosslightshift,
			xfregs.postMtxInfo[i].index, xfregs.postMtxInfo[i].normalize);
	}

	PRIM_LOG("pixel: tev=%d, ind=%d, texgen=%d, dstalpha=%d, alphatest=0x%x", bpmem.genMode.numtevstages+1, bpmem.genMode.numindstages,
		bpmem.genMode.numtexgens, (u32)bpmem.dstalpha.enable, (bpmem.alpha_test.hex>>16)&0xff);
#endif

	u32 usedtextures = 0;
	for (u32 i = 0; i < bpmem.genMode.numtevstages + 1u; ++i)
		if (bpmem.tevorders[i / 2].getEnable(i & 1))
			usedtextures |= 1 << bpmem.tevorders[i/2].getTexMap(i & 1);

	if (bpmem.genMode.numindstages > 0)
		for (unsigned int i = 0; i < bpmem.genMode.numtevstages + 1u; ++i)
			if (bpmem.tevind[i].IsActive() && bpmem.tevind[i].bt < bpmem.genMode.numindstages)
				usedtextures |= 1 << bpmem.tevindref.getTexMap(bpmem.tevind[i].bt);

	// :chiri: nvapi
	float stereoSeparation = 0.0f;
	float scissorX = bpmem.scissorOffset.x;
	float scissorY = bpmem.scissorOffset.y;
	float absoluteWPos = -1.f;

	for (unsigned int i = 0; i < 8; i++)
	{
		if (usedtextures & (1 << i))
		{
			g_renderer->SetSamplerState(i & 3, i >> 2);
			const FourTexUnits &tex = bpmem.tex[i >> 2];
			TextureCache::TCacheEntryBase* tentry = TextureCache::Load(i,
				(tex.texImage3[i&3].image_base/* & 0x1FFFFF*/) << 5,
				tex.texImage0[i&3].width + 1, tex.texImage0[i&3].height + 1,
				tex.texImage0[i&3].format, tex.texTlut[i&3].tmem_offset<<9,
				tex.texTlut[i&3].tlut_format,
				((tex.texMode0[i&3].min_filter & 3) != 0),
				(tex.texMode1[i&3].max_lod + 0xf) / 0x10,
				(tex.texImage1[i&3].image_type != 0));

			if (tentry)
			{
				// 0s are probably for no manual wrapping needed.
				PixelShaderManager::SetTexDims(i, tentry->native_width, tentry->native_height, 0, 0);

				// :chiri: Filter modified EFB buffer copy resources.
				if (tentry->IsEfbCopy())
				{
					// Zelda SS USA
					if (gameId == 'S' + ('O' << 7) + ('U' << 14) + ('E' << 21) + ('0' << 28) + ('1' << 35))
					{
						unsigned int texId = bpmem.tex[i >> 2].texImage0[i & 3].hex << 5;
						// Blurry screen from softening effect.
						if ((tentry->native_width == 114 && tentry->native_height == 152) ||
							(tentry->native_width == 152 && tentry->native_height == 114))
							tentry->BindTransparent(i);
						// Unknown 2D post processing resulting in blurry screen.
						else if ((bpmem.blendmode.blendenable && tentry->format == 6 && tentry->native_width == 608 && tentry->native_height == 456) ||
							// Lower right distortion.
							(bpmem.blendmode.blendenable && tentry->native_width == 272 && tentry->native_height == 232))
							return;
					}

					// Zelda TP EUR
					else if (gameId == 'R' + ('Z' << 7) + ('D' << 14) + ('P' << 21) + ('0' << 28) + ('1' << 35))
					{
						// Fix accumulated Z-Position in Water effect.
						if (bpmem.blendmode.blendenable && i == 0 && tentry->format == 6 && tentry->native_width == 320 && tentry->native_height == 228)
							absoluteWPos = 0;
					}

					// Metroid: Other M
					else if (gameId == 'R' + ('3' << 7) + ('O' << 14) + ('P' << 21) + ('0' << 28) + ('1' << 35))
					{
						// Unknown lighting effect causing extreme color flicker
						static int killCount = 0;
						if (bpmem.blendmode.blendenable && i == 0 && tentry->format == 4 && tentry->native_width == 160 && tentry->native_height == 120)
						{
							tentry->BindTransparent(i);
							killCount = 6;
						}
						// Unknown 2D post processing killing stereo in upper half of screen.
						else if (!bpmem.blendmode.blendenable && i == 0 && tentry->format == 6 && tentry->native_width == 640 && tentry->native_height == 480)
							return;
						// Explosion effect.
						if (i == 0 && (killCount--) > 1 && killCount < 5)
							return;
					}

					// The house of the dead overkill
					else if (g_vertex_manager->gameId == 'R' + ('H' << 7) + ('O' << 14) + ('P' << 21) + ('8' << 28) + ('P' << 35))
					{
						// Skip full screen blur.
						if (i == 1 && tentry->format == 6 && tentry->native_width == 640 && tentry->native_height == 480)
							return;
						// Remove blur effect.
						if ((i == 1 && tentry->format == 6 && tentry->native_width == 80 && tentry->native_height == 60) ||
							(i == 1 && tentry->format == 6 && tentry->native_width == 320 && tentry->native_height == 240))
							tentry->BindTransparent(i);
						// Kill slowmotion effect.
						static int killCount = 0;
						if (i == 0 && tentry->format == 0xb && tentry->native_width == 320 && tentry->native_height == 240)
							killCount = 5;
						if (i == 0 && (killCount--) > 0)
							return;
					}
				}
				else
				{
					// House of the Dead 2&3
					if (g_vertex_manager->gameId == 'R' + ('H' << 7) + ('D' << 14) + ('J' << 21) + ('8' << 28) + ('P' << 35))
					{
						float *pstart = (float*)&xfmem[0];
						// Kill the gun smoke: It's overwriting Z-Buffer.
						if (u32(tentry->hash) == 0xa576d687)
							return;
						// Move shots into scene (shrink size by half to reduce constant z wrongness effect).
						else if ((u32(tentry->hash) == 0x38b9cf3c || u32(tentry->hash) == 0xd63adfe8) && pstart[11] >= -1.2f && pstart[11] < -1.f)
						{
							const float s = 6.f;
							pstart[0] *= s; pstart[1] *= s;	pstart[2] *= s; pstart[3] *= s*2.f;
							pstart[4] *= s; pstart[5] *= s; pstart[6] *= s; pstart[7] *= s*2.f;
							pstart[8] *= s; pstart[9] *= s; pstart[10] *= s; pstart[11] *= s*2.f;
						}
						// Move weapon.
						else if ((u32(tentry->hash) == 0xa1c5db28 || u32(tentry->hash) == 0xec9ca1ec) && pstart[11] > -15.f && pstart[11] < 0)
						{
							const float s = 13.f;
							pstart[0] *= s; pstart[1] *= s;	pstart[2] *= s; pstart[3] *= s;
							pstart[4] *= s; pstart[5] *= s; pstart[6] *= s; pstart[7] *= s;
							pstart[8] *= s; pstart[9] *= s; pstart[10] *= s; pstart[11] *= s;
						}
					}

					// The house of the dead overkill
					else if (g_vertex_manager->gameId == 'R' + ('H' << 7) + ('O' << 14) + ('P' << 21) + ('8' << 28) + ('P' << 35))
					{
						// Remove film grain effect.
						u32 texHash = u32(tentry->hash);
						if (texHash == 0xf711654b ||	// new hash algo: Grain line
							texHash == 0x2bc2a611 ||	// new hash algo: Small grain
							texHash == 0x010bca17 ||	// new hash algo: Small grain
							texHash == 0x5a8cf729 ||	// old hash algo: Grain line
							texHash == 0xd9135ac6 ||    // old hash algo: Small grain
							texHash == 0x9ccb6318)		// old hash algo: Small grain
							tentry->BindTransparent(i);
					}

					// Pikmin 1 GC
					else if (g_vertex_manager->gameId == 'G' + ('P' << 7) + ('I' << 14) + ('E' << 21) + ('0' << 28) + ('1' << 35))
					{
						float *pstart = (float*)&xfmem[0];
						// HUD
						if (pstart[11] < -100 &&
							tentry->hash == 0x00561982fa9f55a7ul ||
							tentry->hash == 0x0122556da94e4366ul ||
							tentry->hash == 0x03fd108f44a4de98ul ||
							tentry->hash == 0x0aaac6bac9a02b9aul ||
							tentry->hash == 0x0d4e455bfc993e13ul ||
							tentry->hash == 0x1378b285eed2f35aul ||
							tentry->hash == 0x16c3483013b11be0ul ||
							tentry->hash == 0x192976e221710ea0ul ||
							tentry->hash == 0x193a73fc8d36d395ul ||
							tentry->hash == 0x1b8cc156ecd427bful ||
							tentry->hash == 0x1cf2673d29d8f941ul ||
							tentry->hash == 0x1d971894770962aeul ||
							tentry->hash == 0x244af5f32625b724ul ||
							tentry->hash == 0x29547cec08392c22ul ||
							tentry->hash == 0x2e82b43b0a73638ful ||
							tentry->hash == 0x2edf4615b78e0947ul ||
							tentry->hash == 0x310c41f033a26fd8ul ||
							tentry->hash == 0x3403d0aba1e6797ful ||
							tentry->hash == 0x365d428ad739773cul ||
							tentry->hash == 0x4565811992b12427ul ||
							tentry->hash == 0x46332cfdeb84eb3eul ||
							tentry->hash == 0x47e9586d4ea36168ul ||
							tentry->hash == 0x4ae30839174287ebul ||
							tentry->hash == 0x5fc8c246747bf32aul ||
							tentry->hash == 0x5a22d94a7e17a06eul ||
							tentry->hash == 0x60440fccee6aea30ul ||
							tentry->hash == 0x61a43b23941c371eul ||
							tentry->hash == 0x61ab6dbe8d1d0e7dul ||
							tentry->hash == 0x624a7f2a1f5203d1ul ||
							tentry->hash == 0x634eb57fdd9216a7ul ||
							tentry->hash == 0x636141563777613aul ||
							tentry->hash == 0x646b7fdda54e4034ul ||
							tentry->hash == 0x6560f52ad8c412aful ||
							tentry->hash == 0x667f7746e08da49dul ||
							tentry->hash == 0x6fcf4c00fab239f4ul ||
							tentry->hash == 0x749bdde0cee06667ul ||
							tentry->hash == 0x7a9feaa5e89576adul ||
							tentry->hash == 0x7cb8e1a68759e0b6ul ||
							tentry->hash == 0x7e25d72c116e0cf6ul ||
							tentry->hash == 0x812da65868cb0261ul ||
							tentry->hash == 0x8f592e7c6146cb67ul ||
							tentry->hash == 0x93a2e214f2c425bbul ||
							tentry->hash == 0x98f32df491a98c49ul ||
							tentry->hash == 0x9d1b1c5097eff1a0ul ||
							tentry->hash == 0x9f0aaab389174711ul ||
							tentry->hash == 0xa19ffe2a3f0755a5ul ||
							tentry->hash == 0xa52da30fc85502cbul ||
							tentry->hash == 0xa7d6cbc475f2f134ul ||
							tentry->hash == 0xaf03b58bcc8389fcul ||
							tentry->hash == 0xaf5f414918232e96ul ||
							tentry->hash == 0xaf6f829212056e11ul ||
							tentry->hash == 0xb19fd0a17e4d8141ul ||
							tentry->hash == 0xb56affd2fb9538d1ul ||
							tentry->hash == 0xbda9faa749ee954ful ||
							tentry->hash == 0xc2f9895e687d306cul ||
							tentry->hash == 0xc4abfcee35b08a6bul ||
							tentry->hash == 0xcb80e10ee73edbd9ul ||
							tentry->hash == 0xcd3279b85d532e92ul ||
							tentry->hash == 0xce6843e04f097b00ul ||
							tentry->hash == 0xce6a801c0c7cc949ul ||
							tentry->hash == 0xd336085349f5efecul ||
							tentry->hash == 0xd86c9fd22f1e540aul ||
							tentry->hash == 0xdd1b4a7ad8f9b90aul ||
							tentry->hash == 0xe1ab6c599615113cul ||
							tentry->hash == 0xe44a2d62b99c1b32ul ||
							tentry->hash == 0xe45c79445543f2a2ul ||
							tentry->hash == 0xe889259c514d026ful ||
							tentry->hash == 0xf9731ac5958a3098ul ||
							tentry->hash == 0xfabbcc0b4280d29cul)
						{
							const float s = 0.38f;
							pstart[0] *= s; pstart[1] *= s;	pstart[2] *= s; pstart[3] *= s;
							pstart[4] *= s; pstart[5] *= s; pstart[6] *= s; pstart[7] *= s;
							pstart[8] *= s; pstart[9] *= s; pstart[10] *= s; pstart[11] *= s;
						}
						// Remove borders.
						else if (tentry->hash == 0xd5278621fbd0fd7bul)
							tentry->BindTransparent(i);
					}

					// Pikmin 2
					else if (g_vertex_manager->gameId == 'R' + ('9' << 7) + ('2' << 14) + ('P' << 21) + ('0' << 28) + ('1' << 35))
					{
					}
				}

				// Stereo debug.
				if (SConfig::GetInstance().m_LocalCoreStartupParameter.bStereoDebug)
				{
					if (checkHash(tentry, i))
						tentry->BindTransparent(i);
				}
			}
			else
				ERROR_LOG(VIDEO, "error loading texture");
		}
	}

	// :chiri: House of the Dead 2&3: Scale objects from far behind to screen position.
	if (g_vertex_manager->gameId == 'R' + ('H' << 7) + ('D' << 14) + ('J' << 21) + ('8' << 28) + ('P' << 35))
	{
		float *pstart = (float*)&xfmem[0];
		// Correct transformation matrix of full screen effects.
		if (pstart[3] == 0.f && pstart[7] == 0.f && pstart[8] == 0.f && pstart[9] == 0.f && pstart[10] == 1.f && pstart[11] <= -0.1)
		{
			const float s = 200.f;
			pstart[0] *= s; pstart[1] *= s;	pstart[2] *= s; pstart[3] *= s;
			pstart[4] *= s; pstart[5] *= s; pstart[6] *= s; pstart[7] *= s;
			pstart[8] *= s; pstart[9] *= s; pstart[10] *= s; pstart[11] *= s;
		}
		// Move amunition.
		else if (pstart[10] > -0.006f && pstart[10] < 0.f && pstart[11] > -0.201 && pstart[11] < -0.199)
		{
			const float s = 80.f;
			pstart[0] *= s; pstart[1] *= s;	pstart[2] *= s; pstart[3] *= s;
			pstart[4] *= s; pstart[5] *= s; pstart[6] *= s; pstart[7] *= s;
			pstart[8] *= s; pstart[9] *= s; pstart[10] *= s; pstart[11] *= s;
		}
	}

	// :chiri: Ikaruga aspect ratio fix.
	else if (g_vertex_manager->gameId == 'G' + ('I' << 7) + ('K' << 14) + ('P' << 21) + ('7' << 28) + ('0' << 35))
	{
		float *pstart = (float*)&xfmem[0];
		pstart[0] *= 0.75f;
		pstart[3] += 80.f;
	}

	// :chiri: Pikmin 1 GC
	else if (g_vertex_manager->gameId == 'G' + ('P' << 7) + ('I' << 14) + ('E' << 21) + ('0' << 28) + ('1' << 35))
	{
		//$$$$		checkHash3();
	}

	// :chiri: nvapi
	if (absoluteWPos != -1.f)
	{
		StereoHandle *sh = (StereoHandle *)DX11::GetStereoHandle();
		NvAPI_Stereo_GetSeparation(*sh, &stereoSeparation);
		NvAPI_Stereo_SetSeparation(*sh, 0.0f);
	}

	// set global constants
	VertexShaderManager::SetConstants();
	PixelShaderManager::SetConstants();

	bool useDstAlpha = !g_ActiveConfig.bDstAlphaPass && bpmem.dstalpha.enable && bpmem.blendmode.alphaupdate
		&& bpmem.zcontrol.pixel_format == PIXELFMT_RGBA6_Z24;

	if(PerfQueryBase::ShouldEmulate())
		g_perf_query->EnableQuery(bpmem.zcontrol.early_ztest ? PQG_ZCOMP_ZCOMPLOC : PQG_ZCOMP);
	g_vertex_manager->vFlush(useDstAlpha);
	if(PerfQueryBase::ShouldEmulate())
		g_perf_query->DisableQuery(bpmem.zcontrol.early_ztest ? PQG_ZCOMP_ZCOMPLOC : PQG_ZCOMP);

	GFX_DEBUGGER_PAUSE_AT(NEXT_FLUSH, true);

	// :chiri: restore custom offset.
	bpmem.scissorOffset.x = scissorX;
	bpmem.scissorOffset.y = scissorY;
	// :chiri: nvapi
	if (stereoSeparation != 0.0f)
	{
		StereoHandle *sh = (StereoHandle *)DX11::GetStereoHandle();
		NvAPI_Stereo_SetSeparation(*sh, stereoSeparation);
	}

	IsFlushed = true;
}

void VertexManager::DoState(PointerWrap& p)
{
	g_vertex_manager->vDoState(p);
}
