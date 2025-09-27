public class NoteFrequence{
    // Il existe une equation qui permet de trouver la frequence des Sons cette methode statique permet de justument de la trouver
    public static double getFrequency(String note) {
        if (note.equalsIgnoreCase("silence")) return 0.0; //Pour les silences, on met la frequence à 0Hz

        String[] notes = {"C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"}; //Les 12 notes de la gamme chromatique
        note = note.toUpperCase().replace("♯", "#"); //Standarditation des #

        String pitch = note.substring(0, note.length() - 1); //On recupere la note de la gamme chromatique ("A" ou bien "C#")
        int octave = Integer.parseInt(note.substring(note.length() - 1)); //On recupere l'octave de la note 

        //Permet de trouver le numéro MIDI de la note, renvoie -1 si la note n'est pas trouvé
        int index = -1;
        for (int i = 0; i < notes.length; i++) {
            if (notes[i].equals(pitch)) {
                index = i;
                break;
            }
        }

        if (index == -1) {
            throw new IllegalArgumentException("Note inconnue : " + note);
        }
        //Permet de trouver la frequence
        int midi = (octave + 1) * 12 + index;
        return 440.0 * Math.pow(2, (midi - 69) / 12.0);
    }
}
