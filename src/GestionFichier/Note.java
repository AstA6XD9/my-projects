/** Les notes constituent une musique, elles sont données par :
 * la date de leur début (en s), leur durée (en s), leur fréquence (en Hz)
 * 
 */

package GestionFichier;

public class Note {
    protected double start;
    protected double duration;
    protected double frequency;

    public Note(double start, double duration, double frequency) {
        this.start = start;
        this.duration = duration;
        this.frequency = frequency;
    }
}
