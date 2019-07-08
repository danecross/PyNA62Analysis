

NA62AnalysisBuilder.py new MyAnalyzer.py

cd ~/Analysis/MyAnalysis/
rm config 
vim config -c '$put=\"analyzers = MyAnalyzer\npreanalyzers = SpectrometerTrackCorrections LKrClusterCorrections GigaTrackerFineCorrections\nexec = MyAnalyzer\"' -c 'wq'

cd ../../SummerProject

