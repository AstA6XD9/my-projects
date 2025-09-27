package allumettes;

import java.util.Random ; 
public class StrategieNaif implements Strategie {
	private Random random = new Random() ; 
	
	@Override
	public int getPrise(Jeu jeu) {
		int prise = random.nextInt(Jeu.PRISE_MAX) + 1 ; 
		if (prise == 0) {
			prise =1;
		}
		return prise;
	}
	


}
