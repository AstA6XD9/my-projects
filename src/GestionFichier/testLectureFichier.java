/** Simple classe de test pour LectureFichier.java */

package GestionFichier;

import java.util.List;

public class testLectureFichier {
    public static void main(String[] args){
        try {

            System.out.println();
            Musique musique = LectureFichier.chargerMusique("Musique1");
            System.out.println("Titre : " + musique.getTitle());
            List<Note> notes = musique.getNotes();
            for (Note n : notes) {
                System.out.println(n.duration);
            }
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}
