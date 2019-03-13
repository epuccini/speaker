#include "speaker.h"

Speaker::Speaker()
{
    QString engine = "default";
    m_speech = new QTextToSpeech(this);
    m_voices = m_speech->availableVoices();
}

Speaker::~Speaker()
{
    delete m_speech;
}

void Speaker::speak(char* text) {
    m_speech->say(QString(text));
}

void Speaker::setVoice(int idx) {
     m_speech->setVoice(m_voices.at(idx));
}

unsigned int Speaker::availableVoices(void) {
    m_voices.count();
}

void Speaker::getVoiceName(char* pszOut) {
    QLocale current = m_speech->locale();
    QString name = current.name();
    strcpy(pszOut, name.toUtf8());
}
