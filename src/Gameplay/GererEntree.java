import GestionFichier.*;

public class GererEntree {
    
    Musique musique;

    public GererEntree(String nomMusique) {
        try {
            this.musique = LectureFichier.chargerMusique(nomMusique);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void lancerLectureMusique() {

    }
}
