/////////////////////////////////////////////////////////
//
//  Speaker.cpp
//  Speaker - container for Sapi-Speechsynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//
/////////////////////////////////////////////////////////
#pragma once 

#include "stdafx.h"
#include <sapi.h>

#include "libspeak.h"

/////////////////////////////////////////////////////////

namespace libspeak
{
	const wchar_t *GetWC(const char *c);

	class Speaker
	{
	public:
		Speaker(char* speech);
		~Speaker();
	
		void SpeakWithText(char* text);
	private:
		ISpVoice * m_pVoice;
	};
}