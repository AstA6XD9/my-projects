package Son;
import javax.sound.sampled.*;
import java.util.Arrays;

public class Microphone {
    private boolean microActif;
    private TargetDataLine microphone;
    private AudioFormat format;
    private double frequenceCapture;

    public Microphone() {
        this.microActif = false;
        this.format = new AudioFormat(44100, 16, 1, true, true);
    }

    public void activer() throws LineUnavailableException {
        if (microActif) return;
        
        DataLine.Info info = new DataLine.Info(TargetDataLine.class, format);
        microphone = (TargetDataLine) AudioSystem.getLine(info);
        microphone.open(format);
        microphone.start();
        microActif = true;
    }

    public void desactiver() {
        if (microphone != null) {
            microphone.stop();
            microphone.close();
        }
        microActif = false;
    }

    public double[] capturerSon() {
        if (!microActif) return new double[0];
        
        byte[] buffer = new byte[4096];
        int bytesRead = microphone.read(buffer, 0, buffer.length);
        
        // Conversion des bytes en amplitudes (valeurs entre -1.0 et 1.0)
        double[] amplitudes = new double[bytesRead / 2];
        for (int i = 0; i < amplitudes.length; i++) {
            short sample = (short) ((buffer[2*i+1] << 8) | buffer[2*i]);
            amplitudes[i] = sample / 32768.0; // Normalisation
        }
        
        // Calcul de la fréquence dominante (version simplifiée)
        frequenceCapture = estimerFrequence(amplitudes);
        
        return amplitudes;
    }

    private double estimerFrequence(double[] amplitudes) {
        // Implémentation basique - pour une vraie analyse utiliser FFT
        double maxAmplitude = 0;
        int zeroCrossings = 0;
        
        for (int i = 0; i < amplitudes.length - 1; i++) {
            maxAmplitude = Math.max(maxAmplitude, Math.abs(amplitudes[i]));
            if (amplitudes[i] * amplitudes[i+1] < 0) {
                zeroCrossings++;
            }
        }
        
        // Estimation très simplifiée de la fréquence
        return (zeroCrossings / 2.0) * (format.getSampleRate() / amplitudes.length);
    }

    public boolean estActif() {
        return microActif;
    }

    public double getFrequenceCapture() {
        return frequenceCapture;
    }
}
