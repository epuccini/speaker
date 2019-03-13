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
    unsigned int availableVoices(void);
    void getVoiceName(char* pszOut);

private:
    QTextToSpeech *m_speech;
    QVector<QVoice> m_voices;
};

#endif // SPEAKER_H
