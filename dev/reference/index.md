# Package index

## Preprocessing

Pool and standardize SAP2011 WAV recordings before analysis

- [`preprocess_sap_wavs()`](https://lxiao06.github.io/LYS/dev/reference/preprocess_sap_wavs.md)
  : Pool and standardize SAP2011 WAV files
- [`as_sap()`](https://lxiao06.github.io/LYS/dev/reference/as_sap.md) :
  Convert a LYS object to an ASAP Sap object

## Object Creation & Management

Functions for creating and managing core LYS objects

- [`create_lys_object()`](https://lxiao06.github.io/LYS/dev/reference/create_lys_object.md)
  : Create a LYS object from a directory of WAV files
- [`get_wav_indices()`](https://lxiao06.github.io/LYS/dev/reference/get_wav_indices.md)
  : Look up metadata row indices for WAV files in a LYS object
- [`remove_wav_files()`](https://lxiao06.github.io/LYS/dev/reference/remove_wav_files.md)
  : Remove specified WAV file(s) from a LYS object
- [`update_lys_paths()`](https://lxiao06.github.io/LYS/dev/reference/update_lys_paths.md)
  : Re-anchor a LYS object to a new data root

## Audio Visualization & Snippet Export

Functions for visualising audio spectrograms and exporting audio clips

- [`visualize_song()`](https://lxiao06.github.io/LYS/dev/reference/visualize_song.md)
  : Visualize a song via ASAP
- [`create_audio_clip()`](https://lxiao06.github.io/LYS/dev/reference/create_audio_clip.md)
  : Create an audio clip via ASAP

## Vocalization Detection

Functions for detecting vocalizations from WAV recordings

- [`detect_vocalization()`](https://lxiao06.github.io/LYS/dev/reference/detect_vocalization.md)
  : Detect vocalizations in audio

## Template Match Classification

Functions for registering templates and detecting matches

- [`create_template()`](https://lxiao06.github.io/LYS/dev/reference/create_template.md)
  : Create a vocalization template
- [`detect_template()`](https://lxiao06.github.io/LYS/dev/reference/detect_template.md)
  : Run template-based vocalization detection

## Labeling & Sequence Annotation

Functions for assigning vocal labels and annotating sequences

- [`label_vocalization()`](https://lxiao06.github.io/LYS/dev/reference/label_vocalization.md)
  : Label detected vocalizations using template-match rules
- [`annotate_vocalizations()`](https://lxiao06.github.io/LYS/dev/reference/annotate_vocalizations.md)
  : Apply Sequence Rules to Annotate Vocalizations

## Motif Extraction & Segment Visualization

Functions for extracting fixed-window motifs and visualizing segments

- [`find_motif()`](https://lxiao06.github.io/LYS/dev/reference/find_motif.md)
  : Extract vocalization motifs
- [`visualize_segments()`](https://lxiao06.github.io/LYS/dev/reference/visualize_segments.md)
  : Visualize Audio Segments or Motifs

## Vocalization Trajectory & Dimensionality Reduction

Functions for trajectory matrix construction, PCA, and condition
separation

- [`create_trajectory_matrix()`](https://lxiao06.github.io/LYS/dev/reference/create_trajectory_matrix.md)
  : Create Spectrogram Matrices for Trajectory Analysis
- [`run_pca()`](https://lxiao06.github.io/LYS/dev/reference/run_pca.md)
  : Run Principal Component Analysis on Trajectory Matrix
- [`trajectory_timecourse()`](https://lxiao06.github.io/LYS/dev/reference/trajectory_timecourse.md)
  : Trajectory Timecourse Analysis
- [`trajectory_separation()`](https://lxiao06.github.io/LYS/dev/reference/trajectory_separation.md)
  : Trajectory Separation and Discriminability Analysis
- [`plot_trajectory_discrimination()`](https://lxiao06.github.io/LYS/dev/reference/plot_trajectory_discrimination.md)
  : Plot Trajectory Discrimination Results

## Exporting & Mapping

Functions for exporting clipped events and mapping session timelines

- [`export_vocalizations()`](https://lxiao06.github.io/LYS/dev/reference/export_vocalizations.md)
  : Export vocalization clips to per-label subdirectories
- [`map_vocalization_sessions()`](https://lxiao06.github.io/LYS/dev/reference/map_vocalization_sessions.md)
  : Map labeled vocalizations across recording sessions
- [`animate_vocalization_sessions()`](https://lxiao06.github.io/LYS/dev/reference/animate_vocalization_sessions.md)
  : Animate Vocalization Sessions with Sliding Window

## Sequence Statistics & Reciprocal Analysis

Statistical analysis of vocalization transitions and latencies

- [`compute_onset_latency()`](https://lxiao06.github.io/LYS/dev/reference/compute_onset_latency.md)
  : Compute Onset Latency Distribution Between Vocalization Pairs
- [`permutation_transition_test()`](https://lxiao06.github.io/LYS/dev/reference/permutation_transition_test.md)
  : Permutation Test for Directional Transition Asymmetry
- [`plot_reciprocal_latency()`](https://lxiao06.github.io/LYS/dev/reference/plot_reciprocal_latency.md)
  : Plot Reciprocal Onset Latency Distributions
- [`conditional_rate_ratio()`](https://lxiao06.github.io/LYS/dev/reference/conditional_rate_ratio.md)
  : Conditional Rate Ratio for Vocalization Transitions

## Population-Level Analysis

Pool LYS objects and test population-level vocalization dynamics

- [`pool_lys_session_maps()`](https://lxiao06.github.io/LYS/dev/reference/pool_lys_session_maps.md)
  : Pool vocalization session maps from multiple animals
- [`population_transition_test()`](https://lxiao06.github.io/LYS/dev/reference/population_transition_test.md)
  : Population-level test for directional transition asymmetry
- [`population_permutation_test()`](https://lxiao06.github.io/LYS/dev/reference/population_permutation_test.md)
  : Animal-respecting permutation test for population-level transition
  asymmetry
- [`population_rate_ratio()`](https://lxiao06.github.io/LYS/dev/reference/population_rate_ratio.md)
  : Population-level Conditional Rate Ratio Analysis
- [`population_peth()`](https://lxiao06.github.io/LYS/dev/reference/population_peth.md)
  : Peri-Event Time Histogram (PETH) and Multi-Scale Triggering Analysis
- [`population_shuffled_peth()`](https://lxiao06.github.io/LYS/dev/reference/population_shuffled_peth.md)
  : Shuffled Surrogate PETH for Testing Pointwise Time-Window
  Significance
- [`population_point_process_glm()`](https://lxiao06.github.io/LYS/dev/reference/population_point_process_glm.md)
  : Point-Process GLM for Disentangling Triggering vs. Refractory
  Suppression
