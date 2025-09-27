package allumettes;

import java.util.Scanner;

public class StrategieHumain implements Strategie {
    private Scanner scanner = new Scanner(System.in);
    private String nom;

    public StrategieHumain(String nom) {
        this.nom = nom;
    }

    public int getPrise(Jeu jeu) {
        while (true) {
            System.out.print(nom + ", combien d'allumettes ? ");
            try {
                int prise = scanner.nextInt();
                if (prise < 1 || prise > Jeu.PRISE_MAX) {
                    System.out.println("Impossible ! Nombre invalide : " + prise + 
                                     (prise < 1 ? " (< 1)" : " (> " + Jeu.PRISE_MAX + ")"));
                    continue;
                }
                return prise;
            } catch (java.util.InputMismatchException e) {
                System.out.println("Vous devez donner un entier.");
                scanner.next(); // Vide le buffer
            }
        }
    }
}