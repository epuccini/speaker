#include "../../libspeak.h"
#include "speaker.h"

///////////
// Speaker
//
static Speaker* speakerLocal;

void init_speaker()  {
    speakerLocal =  new Speaker();
}

void speak(char* text)  {
    speakerLocal->speak(text);
}

void set_voice(int index)  {
    speakerLocal->setVoice(index);
}

unsigned int available_voices_count(void)  {
    return speakerLocal->availableVoices();
}
void set_language(int index)  {
    speakerLocal->setLanguage(index);
}

unsigned int available_languages_count(void)  {
  return speakerLocal->availableLanguages();
}

void get_voice_name(unsigned int idx, char* pszOut)  {
    speakerLocal->setVoice(idx);
    speakerLocal->getVoiceName(pszOut);
}

void cleanup_speaker(void)  {
    delete speakerLocal;
}

void mainloop_speaker(void* speaker)  {

}

bool is_speaking(void* speaker)  {
    QTextToSpeech::State state;
    if (state == QTextToSpeech::Speaking) {
        return true;
    } else {
        return false;
    }
}

///////////////
// Speaker OO
//
void* make_speaker()  {
    Speaker* speaker =  new Speaker();
    return (void*) speaker;
}

void speak_with(void* speaker, char* text) {
    ((Speaker*)speaker)->speak(text);
}

void set_voice_with(void* speaker, int index)  {
    ((Speaker*)speaker)->setVoice(index);
}

void cleanup_with(void* speaker)  {
    delete ((Speaker*)speaker);
}

void register_will_speak_word_callback(void* speaker, wsw_callback cb)  {

}

void register_will_speak_phoneme_callback(void* speaker, wsp_callback cb)  {

}

void register_did_finish_speaking_callback(void* speaker, dfs_callback cb)  {

}

/////////////////
// Recognizer OO
//
void* make_listener()  {

}

void start_listening(void* listener)  {

}

void stop_listening(void* listener)  {

}

void add_command(void* listener, char*)  {

}

void cleanup_listener(void* listener)  {

}

void mainloop_listener(void* listener)  {

}

bool is_listening(void* listener)  {

}

void register_did_recognize_command_callback(void* listener, drc_callback cb) {

}
