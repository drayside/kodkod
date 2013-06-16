package kodkod.multiobjective.statistics;

import kodkod.ast.Formula;
import kodkod.multiobjective.MetricPoint;

public class IndividualStats {
	private StatKey SatCallType;
	private long TranslationTime;
	private long SolvingTime;
	
	private MetricPoint ObjectiveValueReceived;
	private Formula improvementConstraints;
	
	public IndividualStats(StatKey SatCallType, long TranslationTime, long SolvingTime, MetricPoint ObjectiveValueReceived, Formula improvementConstraints){
		this.SatCallType = SatCallType;
		this.TranslationTime = TranslationTime;
		this.SolvingTime = SolvingTime;
		if (this.isRegularSatCall()){
			this.ObjectiveValueReceived = ObjectiveValueReceived;
		}
		this.improvementConstraints = improvementConstraints;
	}
	public boolean isRegularSatCall() { 
		return this.SatCallType == StatKey.REGULAR_SAT_CALL;
	} 	

	public boolean isRegularUnSatCall() { 
		return this.SatCallType == StatKey.REGULAR_UNSAT_CALL;
	} 	
	
	public StatKey getSatCallType(){
		return this.SatCallType;
	}
	
	public long getTranslationTime() {
		return this.TranslationTime;
	}
	public long getSolvingTime() {
		return this.SolvingTime;
	}
	
	public static String getHeaderLine(){
		String CSVHeader = "SatCallType, TranslationTime, SolvingTime, LabelMetricPointValues, MetricPointValues, TightOrBoostedImprovementConstraints" ;
		return CSVHeader;
	}
	public String toString(){
		String CSVSummary = "";
		
		CSVSummary += this.SatCallType.toString();

		CSVSummary += "," + this.getTranslationTime();
		CSVSummary += "," + this.getSolvingTime();
		CSVSummary += ",MetricPointValues" ;

		if (this.isRegularSatCall()){
			CSVSummary += "," + this.ObjectiveValueReceived.values().toString();
		}else{
			CSVSummary += ",";			
		}
		
		CSVSummary += ",ImprovementConstraintsUsed" ;
		
		if (this.improvementConstraints != null){
			CSVSummary += "," + this.improvementConstraints.toString();
		}else{
			CSVSummary += ",";			
		}
		
		return CSVSummary;
	}
}
