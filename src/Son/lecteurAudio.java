package Son;

import javax.sound.sampled.*;
import java.io.File;
import java.io.IOException;

public class lecteurAudio {
    private Clip clip;
    private FloatControl volumeControl;
    private boolean enLecture;
    private String fichierActuel;
    private Microphone microphone;

    public lecteurAudio() {
        this.microphone = new Microphone();
    }

    public void jouerMusique(String cheminFichier) throws SonException {
        arreterMusique();

        try {
            AudioInputStream audioStream = AudioSystem.getAudioInputStream(new File(cheminFichier));
            clip = AudioSystem.getClip();
            clip.open(audioStream);
            
            volumeControl = (FloatControl) clip.getControl(FloatControl.Type.MASTER_GAIN);
            fichierActuel = cheminFichier;
            
            clip.start();
            enLecture = true;
            microphone.activer();
            
        } catch (UnsupportedAudioFileException | IOException | LineUnavailableException e) {
            throw new SonException("Erreur lors de la lecture audio: " + e.getMessage());
        }
    }

    public void pauseMusique() {
        if (clip != null && clip.isRunning()) {
            clip.stop();
            enLecture = false;
        }
    }

    public void reprendreMusique() {
        if (clip != null && !clip.isRunning()) {
            clip.start();
            enLecture = true;
        }
    }

    public void arreterMusique() {
        if (clip != null) {
            clip.stop();
            clip.close();
            enLecture = false;
            microphone.desactiver();
        }
    }

    public void modifierVolume(int volume) {
        if (volumeControl != null) {
            float min = volumeControl.getMinimum();
            float max = volumeControl.getMaximum();
            float valeur = min + (max - min) * volume / 100f;
            volumeControl.setValue(valeur);
        }
    }

    public int getVolume() {
        if (volumeControl == null) return 50;
        float min = volumeControl.getMinimum();
        float max = volumeControl.getMaximum();
        return (int) ((volumeControl.getValue() - min) / (max - min) * 100);
    }

    public boolean estEnLecture() {
        return enLecture;
    }

    public Microphone getMicrophone() {
        return microphone;
    }
}
