# Wave Function Collapse

## How to Develop

1. [have Nix](https://nixos.org/nix/) (it's available for Mac and Linux)
2. [have direnv](https://direnv.net)
3. `cd` here
4. `direnv allow`
5. `./develop.sh`
6. Go to [localhost:8000](http://localhost:8000). The browser will reload automatically when you make changes.

## To Do / Done

- [x] Read the input bitmap and count NxN patterns.
  - [ ] (optional) Augment pattern data with rotations and reflections.
- [x] Create an array with the dimensions of the output (called "wave" in the source). Each element of this array represents a state of an NxN region in the output. A state of an NxN region is a superposition of NxN patterns of the input with boolean coefficients (so a state of a pixel in the output is a superposition of input colors with real coefficients). False coefficient means that the corresponding pattern is forbidden, true coefficient means that the corresponding pattern is not yet forbidden.
- [ ] Initialize the wave in the completely unobserved state, i.e. with all the boolean coefficients being true.
  - Repeat the following steps:
  - Observation:
    - [ ] Find a wave element with the minimal nonzero entropy. If there is no such elements (if all elements have zero or undefined entropy) then break the cycle (4) and go to step (5).
    - [ ] Collapse this element into a definite state according to its coefficients and the distribution of NxN patterns in the input.
  - Propagation: propagate information gained on the previous observation step.
- [ ] By now all the wave elements are either in a completely observed state (all the coefficients except one being zero) or in the contradictory state (all the coefficients being zero). In the first case return the output. In the second case finish the work without returning anything.

## Links

- [Helpful Explanation of the Algorithm in Rust (long)](https://gridbugs.org/wave-function-collapse/)
- [Original Repo](https://github.com/mxgmn/WaveFunctionCollapse)
- [Interactive Browser Simulation in Unity](http://oskarstalberg.com/game/wave/wave.html)
