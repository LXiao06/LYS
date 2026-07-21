# Package index

## Preprocessing

Pool and standardize SAP2011 WAV recordings before analysis

- [`preprocess_sap_wavs()`](https://lxiao06.github.io/LYS/reference/preprocess_sap_wavs.md)
  : Pool and standardize SAP2011 WAV files
- [`as_sap()`](https://lxiao06.github.io/LYS/reference/as_sap.md) :
  Convert a LYS object to an ASAP Sap object

## Object Creation & Management

Functions for creating and managing core LYS objects

- [`create_lys_object()`](https://lxiao06.github.io/LYS/reference/create_lys_object.md)
  : Create a LYS object from a directory of WAV files
- [`get_wav_indices()`](https://lxiao06.github.io/LYS/reference/get_wav_indices.md)
  : Look up metadata row indices for WAV files in a LYS object

## Audio Visualization & Snippet Export

Functions for visualising audio spectrograms and exporting audio clips

- [`visualize_song()`](https://lxiao06.github.io/LYS/reference/visualize_song.md)
  : Visualize a song via ASAP
- [`create_audio_clip()`](https://lxiao06.github.io/LYS/reference/create_audio_clip.md)
  : Create an audio clip via ASAP

## Vocalization Detection

Functions for detecting vocalizations from WAV recordings

- [`detect_vocalization()`](https://lxiao06.github.io/LYS/reference/detect_vocalization.md)
  : Detect vocalizations in audio

## Template Match Classification

Functions for registering templates and detecting matches

- [`create_template()`](https://lxiao06.github.io/LYS/reference/create_template.md)
  : Create a vocalization template
- [`detect_template()`](https://lxiao06.github.io/LYS/reference/detect_template.md)
  : Run template-based vocalization detection

## Labeling & Sequence Annotation

Functions for assigning vocal labels and annotating sequences

- [`label_vocalization()`](https://lxiao06.github.io/LYS/reference/label_vocalization.md)
  : Label detected vocalizations using template-match rules
- [`annotate_vocalizations()`](https://lxiao06.github.io/LYS/reference/annotate_vocalizations.md)
  : Apply Sequence Rules to Annotate Vocalizations

## Exporting & Mapping

Functions for exporting clipped events and mapping session timelines

- [`export_vocalizations()`](https://lxiao06.github.io/LYS/reference/export_vocalizations.md)
  : Export vocalization clips to per-label subdirectories
- [`map_vocalization_sessions()`](https://lxiao06.github.io/LYS/reference/map_vocalization_sessions.md)
  : Map labeled vocalizations across recording sessions
- [`animate_vocalization_sessions()`](https://lxiao06.github.io/LYS/reference/animate_vocalization_sessions.md)
  : Animate Vocalization Sessions with Sliding Window

## Sequence Statistics & Reciprocal Analysis

Statistical analysis of vocalization transitions and latencies

- [`compute_onset_latency()`](https://lxiao06.github.io/LYS/reference/compute_onset_latency.md)
  : Compute Onset Latency Distribution Between Vocalization Pairs
- [`permutation_transition_test()`](https://lxiao06.github.io/LYS/reference/permutation_transition_test.md)
  : Permutation Test for Directional Transition Asymmetry
- [`plot_reciprocal_latency()`](https://lxiao06.github.io/LYS/reference/plot_reciprocal_latency.md)
  : Plot Reciprocal Onset Latency Distributions
- [`conditional_rate_ratio()`](https://lxiao06.github.io/LYS/reference/conditional_rate_ratio.md)
  : Conditional Rate Ratio for Vocalization Transitions
