# Contributing to Athlytics

## How to Contribute

We follow a "GitHub Flow" like process where contributions are made through Pull Requests. To make a contribution, please follow these steps:

1.  **Check for Existing Issues:** Before starting work on a new feature or bug fix, please check the [Issues tab](https://github.com/HzaCode/Athlytics/issues) to see if someone else has already reported the issue or is working on it.

2.  **Open an Issue (If Necessary):**
    *   **For Bug Reports:** If you find a bug, please open a new issue. Describe the bug clearly, including steps to reproduce it, what you expected to happen, and what actually happened. Include relevant version information for Athlytics and R.
    *   **For Feature Requests:** If you have an idea for a new feature, please open an issue to discuss it. This allows us to discuss the potential feature and ensure it aligns with the project's goals before significant development effort is made.
    *   **For Other Contributions:** If you want to contribute in other ways (e.g., documentation improvements, refactoring), it's still a good idea to open an issue first to discuss your proposed changes.

3.  **Fork the Repository:** Fork the [Athlytics repository](https://github.com/HzaCode/Athlytics) to your own GitHub account.

4.  **Create a Branch:** Create a new branch in your forked repository for your contribution. Choose a descriptive branch name (e.g., `fix-plot-bug`, `add-new-metric`).

5.  **Make Your Changes:** Make your changes in your branch. Ensure that your code follows the existing style and that you add relevant tests for new functionality or bug fixes.
    *   If you add new functions, please include Roxygen documentation.
    *   Run `devtools::check()` locally to ensure your changes pass all checks.

6.  **Commit Your Changes:** Commit your changes with a clear and descriptive commit message.

7.  **Push to Your Fork:** Push your changes to your forked repository.

8.  **Open a Pull Request (PR):**
    *   Go to the original athlytics repository and open a Pull Request from your branch to the `main` branch (or the relevant development branch if specified).
    *   In your PR description, clearly describe the changes you have made and **link to the issue you are addressing** (e.g., "Closes #123" or "Fixes #456").
    *   Ensure your PR passes any automated checks (e.g., GitHub Actions).

9.  **Discuss and Iterate:** Be prepared to discuss your changes and make further modifications based on feedback from the maintainers.

## Code of Conduct

By participating in this project, you are expected to uphold our [Code of Conduct](CODE_OF_CONDUCT.md).

## Questions?

Open an issue for questions or clarifications. 
