package Son;

public class AnalyseurChant {
    private static final double TOLERANCE_FREQUENCE = 10.0; // Hz
    private static final double TOLERANCE_TEMPS = 0.2; // secondes
    
    public double analyserPerformance(double frequenceChante, double frequenceReference, 
                                    double tempsChante, double tempsReference) {
        // Score de justesse (0-100)
        double scoreFrequence = calculerScoreFrequence(frequenceChante, frequenceReference);
        
        // Score de timing (0-100)
        double scoreTiming = calculerScoreTiming(tempsChante, tempsReference);
        
        // Moyenne pondérée
        return (scoreFrequence * 0.7 + scoreTiming * 0.3);
    }
    
    private double calculerScoreFrequence(double freqChante, double freqRef) {
        double difference = Math.abs(freqChante - freqRef);
        if (difference < TOLERANCE_FREQUENCE) {
            return 100;
        }
        return Math.max(0, 100 - (difference - TOLERANCE_FREQUENCE) * 2);
    }
    
    private double calculerScoreTiming(double tempsChante, double tempsRef) {
        double difference = Math.abs(tempsChante - tempsRef);
        if (difference < TOLERANCE_TEMPS) {
            return 100;
        }
        return Math.max(0, 100 - (difference - TOLERANCE_TEMPS) * 50);
    }
}

