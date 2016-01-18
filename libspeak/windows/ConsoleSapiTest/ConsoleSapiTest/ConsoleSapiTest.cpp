// ConsoleSapiTest.cpp : Definiert den Einstiegspunkt für die Konsolenanwendung.
//

#include <stdafx.h>
#include <sapi.h>

int main(int argc, char* argv[])
{
    ISpVoice * pVoice = NULL;

    if (FAILED(::CoInitialize(NULL)))
        return FALSE;

    HRESULT hr = CoCreateInstance(CLSID_SpVoice, NULL, CLSCTX_ALL, IID_ISpVoice, (void **)&pVoice);
    if( SUCCEEDED( hr ) )
    {
        hr = pVoice->Speak(L"Hello world Edward. Nice to see you", 0, NULL);

        // Change pitch
        hr = pVoice->Speak(L"This sounds normal <pitch middle = '-10'/> but the pitch drops half way through", SPF_IS_XML, NULL );
        pVoice->Release();
        pVoice = NULL;
    }
    ::CoUninitialize();
    return TRUE;
}