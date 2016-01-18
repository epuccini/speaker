/////////////////////////////////////////////////////////
//
//  Speaker.cpp
//  Speaker - container for Sapi-Speechsynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//
/////////////////////////////////////////////////////////

#include "Speaker.h"

/////////////////////////////////////////////////////////

namespace libspeak
{
	const wchar_t *GetWC(const char *c)
	{
		const size_t cSize = strlen(c)+1;
		size_t cResultSize = 0;
		wchar_t* wc = new wchar_t[cSize];
		mbstowcs_s(&cResultSize, wc, cSize, c, cSize);

		return wc;
	}

	Speaker::Speaker(char* speech)
	{
		if (SUCCEEDED(::CoInitialize(NULL)))
		{
			HRESULT hr = CoCreateInstance(CLSID_SpVoice, NULL, CLSCTX_ALL, IID_ISpVoice, (void **)&this->m_pVoice);
		}
	}

	Speaker::~Speaker()
	{
		::CoUninitialize();
		m_pVoice->Release();
		m_pVoice = NULL;
	}

	void Speaker::SpeakWithText(char* text)
	{
		if(m_pVoice != NULL)
		{
			LPCWSTR wtext = GetWC(text);
			HRESULT hr = m_pVoice->Speak(wtext, 0, NULL);
		}
		return;
	}
}