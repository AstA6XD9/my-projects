public class NoteRythme{
    public String note;       // ex: "C4", "A4", "silence"
    public long startTimeMs;  // en millisecondes
    public long durationMs;   // en millisecondes

    public NoteRythme(String note, long startTimeMs, long durationMs) {
        this.note = note;
        this.startTimeMs = startTimeMs;
        this.durationMs = durationMs;
    }

    @Override
    public String toString() {
        return note + " : " + startTimeMs + " ms → " + durationMs + " ms";
    }
}
