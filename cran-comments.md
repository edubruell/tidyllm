Test environment: local macOS install, R CMD check --as-cran.

R CMD check result: 0 errors, 0 warnings, 3 notes.

Two notes are local-environment artifacts that will not occur on CRAN's build machines: an offline clock note ("unable to verify current time") and an HTML Tidy version note from an outdated local Tidy binary.

The remaining note flags URL issues in a vignette. These URLs point to the account pages where a user obtains the API key needed to use the package with each provider, so they are functional documentation links rather than incidental references. One URL had moved and has been updated. The others are 403 responses from openai.com and platform.openai.com; those hosts return 403 to any automated client, confirmed with a plain curl request using a browser user agent, so this is bot blocking on their end rather than a broken link.

Summary of changes: this release updates the Claude provider for changes in the Anthropic Messages API affecting current generation models, covering extended thinking configuration, sampling parameter support, and structured output requests. It also adds optional prompt caching for Claude requests and batches, with cache usage reported in request metadata. A few other provider defaults and documentation links were refreshed to match current upstream APIs.

There are no reverse dependencies.
