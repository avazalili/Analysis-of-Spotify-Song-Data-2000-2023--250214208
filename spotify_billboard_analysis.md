
# IJC437 – Spotify / Billboard Hot 100 Analysis (2000–2023)

## Brief Introduction
This project investigates patterns in popular music using Billboard Hot 100 songs (2000–2023) combined with Spotify audio features.

## Research Questions
1. Can Spotify audio features predict whether a song becomes a Top 10 hit?
2. Can hit songs be clustered into groups based on audio features?
3. What trends over time are visible in charting music (e.g., collaborations)?

## Key Findings (Summary)
- Audio features alone provide limited prediction ability for Top-10 vs not (within already-popular songs).
- Clustering identifies meaningful groups such as upbeat dance tracks vs acoustic ballads.
- Collaboration rates increase significantly over time.

## How to Run
1. Install R and RStudio
2. Put dataset here: data/Spotify_data.xlsx
3. Run: code/01_analysis.R
4. Outputs:
   - Figures saved in figures/
   - Model results in outputs/model_results.txt
   - Cluster profiles in outputs/cluster_profiles.csv
