/** Une musique est composée du titre, du bpm et d'une liste de notes
 * 
 */

package GestionFichier;

import java.util.List;

public class Musique {
    private String title;
    private int bpm;
    private List<Note> notes;

    public Musique(String title, int bpm, List<Note> notes) {
        this.title = title;
        this.bpm = bpm;
        this.notes = notes;
    }

    public String getTitle() {
        return this.title;
    }

    public int getBPM() {
        return this.bpm;
    }

    public List<Note> getNotes() {
        return this.notes;
    }
}
