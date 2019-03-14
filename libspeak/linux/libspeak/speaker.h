#ifndef SPEAKER_H
#define SPEAKER_H

#include <QTextToSpeech>
#include <QVector>

class Speaker : QObject
{
    Q_OBJECT

public:
    Speaker();
    ~Speaker();

    void speak(char* text);
    void setVoice(int idx);
    unsigned int availableLanguages(void);
    void setLanguage(int idx);
    unsigned int availableVoices(void);
    void getVoiceName(char* pszOut);

private:
    QTextToSpeech *m_speech;
    QVector<QVoice> m_voices;
    QVector<QString> m_engines;
    QVector<QLocale> m_locales;
};

#endif // SPEAKER_H
