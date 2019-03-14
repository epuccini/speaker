#include "speaker.h"

Speaker::Speaker()
{
    m_speech = new QTextToSpeech(this);
    m_voices = m_speech->availableVoices();

    // get engines
    foreach (QString engine, QTextToSpeech::availableEngines())
       m_engines.push_back(engine);

    // get locales
    m_locales = m_speech->availableLocales();
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
    return m_voices.size();
}

void Speaker::setLanguage(int idx) {
    m_speech->setLocale(m_locales.at(idx));
}

unsigned int Speaker::availableLanguages(void) {
    return m_locales.size();
}

void Speaker::getVoiceName(char* pszOut) {
    QLocale current = m_speech->locale();
    QString name = current.languageToString(current.language());
    strcpy(pszOut, name.toUtf8());
}
