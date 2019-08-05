////////////////////////////////////////////////
//
//  speak.cpp
//  speak - c-  and cpp-interface for 
//  creating cffi to lisp
//
//  Created by Edward Puccini on 17.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////

////////////////////////////////////////////////

#include "Speaker.h"

////////////////////////////////////////////////

static ISpVoice *pVoice = NULL;

////////////////////////////////////////////////

BOOL APIENTRY DllMain(
	HMODULE hModule, 
	DWORD ul_reason_for_call, 
	LPVOID lpReserved)
{
    switch (ul_reason_for_call)
    {
    case DLL_PROCESS_ATTACH:
      //  For optimization.
      DisableThreadLibraryCalls( hModule );
      break;
    case DLL_THREAD_ATTACH:
		break;
    case DLL_THREAD_DETACH:
		break;
    case DLL_PROCESS_DETACH:
        break;
    }
	return TRUE;
}

////////////////////////////////////////////////
//
// Function for fast setup speaking
// Only single speaker possible, but
// offering a simple c-interface
//

void init_speaker()
{
	if (SUCCEEDED(::CoInitialize(NULL)))
	{
		HRESULT hr = CoCreateInstance(CLSID_SpVoice, NULL, CLSCTX_ALL, IID_ISpVoice, (void **)&pVoice);
	}
}

void speak(char* text)
{
	if (pVoice != NULL)
	{
		LPCWSTR ttext = libspeak::GetWC(text);
		HRESULT hr = pVoice->Speak(ttext, 0, NULL);
	}
	return;
}

void set_voice(int index)
{
}

unsigned int available_voices_count(void)
{
	return 0;
}

void set_language(int index)
{
}

unsigned int available_languages_count(void)
{
	return 0;
}

void get_voice_name(unsigned int idx, char* pszOut)
{
}

void cleanup_speaker()
{
	::CoUninitialize();
	pVoice->Release();
	pVoice = NULL;
}

void mainloop_speaker(void* speaker)
{

}

bool is_speaking(void* speaker)
{
	return true;
}

////////////////////////////////////////////////
//
// Function to create Speaker instances
// This is useful for handling different speakers
// at once
//

void* make_speaker()
{
	libspeak::Speaker *pSpeaker = new libspeak::Speaker("");
	return (void*)pSpeaker;
}

void speak_with(void* speaker, char* text)
{
	libspeak::Speaker* pSpeaker = (libspeak::Speaker*)speaker;
	pSpeaker->SpeakWithText(text);
}

void set_voice_with(void* speaker, int index)
{
}

void cleanup_with(void* speaker)
{
	libspeak::Speaker* pSpeaker = (libspeak::Speaker*)speaker;
	delete pSpeaker;
}

////////////////////////////////////////////////
//
// Lisp callbacks can be registered here and
// will be called in Speaker-instance
//
void register_will_speak_word_callback(void* speaker, wsw_callback cb)
{
}

void register_will_speak_phoneme_callback(void* speaker, wsp_callback cb)
{
}

void register_did_finish_speaking_callback(void* speaker, dfs_callback cb)
{
}

void* make_listener()
{
	return NULL;
}

void start_listening(void* listener)
{
}

void stop_listening(void* listener)
{
}

void add_command(void* listener, char*)
{
}

void cleanup_listener(void* listener)
{
}

void mainloop_listener(void* listener)
{
}

bool is_listening(void* listener)
{
	return true;
}

void register_did_recognize_command_callback(void* listener, drc_callback cb)
{
}

