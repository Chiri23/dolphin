// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "AudioCommon/AudioCommon.h"
#include "AudioCommon/Mixer.h"

class DSPHLE;

class HLEMixer : public CMixer
{
public:
	HLEMixer(DSPHLE *dsp_hle, unsigned int AISampleRate = 48000, unsigned int DACSampleRate = 48000, unsigned int BackendSampleRate = 32000)
		: CMixer(AISampleRate, DACSampleRate, BackendSampleRate), m_DSPHLE(dsp_hle) {};

	virtual void Premix(short *samples, unsigned int numSamples) override;
private:
	DSPHLE *m_DSPHLE;
};
