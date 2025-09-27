import java.io.*;
import java.util.*;

public class Parole{
    private int bpm = 120;  // par défaut
    private String signature = "4/4";  // pas encore utilisée ici

    public List<NoteRythme> parse(String filepath) throws IOException {
        List<NoteRythme> notes = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(filepath));

        String line;
        double beatDurationMs = 60000.0 / bpm;  // une noire en ms
        long currentTime = 0;

        while ((line = reader.readLine()) != null) {
            line = line.trim();
            if (line.isEmpty() || line.startsWith("#")) continue;

            if (line.startsWith("BPM:")) {
                bpm = Integer.parseInt(line.substring(4).trim());
                beatDurationMs = 60000.0 / bpm;
            } else if (line.startsWith("SIGNATURE:")) {
                signature = line.substring(10).trim();
            } else {
                String[] parts = line.split(" ");
                if (parts.length != 2) continue;

                String note = parts[0];
                String rythme = parts[1];

                double beats = getBeatsFromRythme(rythme);
                long duration = (long) (beats * beatDurationMs);

                notes.add(new NoteRythme(note, currentTime, duration));
                currentTime += duration;
            }
        }

        reader.close();
        return notes;
    }

    private double getBeatsFromRythme(String rythme) {
        switch (rythme.toLowerCase()) {
            case "ronde": return 4.0;
            case "blanche": return 2.0;
            case "noire": return 1.0;
            case "croche": return 0.5;
            case "double-croche": return 0.25;
            default:
                throw new IllegalArgumentException("Rythme inconnu : " + rythme);
        }
    }
}
