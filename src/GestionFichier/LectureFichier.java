/** Cette classe permet de lire le contenu de MusiqueN.json
 * et de renvoyer un objet Musique contenu les informations
 * 
 */

package GestionFichier;

import org.json.JSONArray;
import org.json.JSONObject;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class LectureFichier {

    public static Musique chargerMusique(String nomMusique) throws Exception {

        // chemin est l'URL du fichier de donnees de musique, depuis la racine livrables
        Path chemin = Paths.get("livrables/src/resources/MusiquesData", nomMusique + ".json");
        String musiqueData = Files.readString(chemin);
        JSONObject json = new JSONObject(musiqueData);

        String titre = json.getString("title");
        int bpm = json.getInt("bpm");
        JSONArray notesJson = json.getJSONArray("notes");

        List<Note> notes = new ArrayList<>();
        for (int i = 0; i < notesJson.length(); i++) {
            JSONObject obj = notesJson.getJSONObject(i);
            notes.add(new Note(obj.getDouble("start"), obj.getDouble("duration"), obj.getDouble("frequency")));
        }

    return new Musique(titre, bpm, notes);

    }

}
