structure(list(method = "POST", url = "https://api.openai.com/v1/chat/completions", 
    status_code = 200L, headers = structure(list(date = "Fri, 21 Mar 2025 11:16:55 GMT", 
        `content-type` = "application/json", `access-control-expose-headers` = "X-Request-ID", 
        `openai-organization` = "user-j2dlcmauhfkzofrqjehceoc7", 
        `openai-processing-ms` = "420", `openai-version` = "2020-10-01", 
        `x-ratelimit-limit-requests` = "5000", `x-ratelimit-limit-tokens` = "800000", 
        `x-ratelimit-remaining-requests` = "4999", `x-ratelimit-remaining-tokens` = "799975", 
        `x-ratelimit-reset-requests` = "12ms", `x-ratelimit-reset-tokens` = "1ms", 
        `x-request-id` = "req_078300a05ac8d8b7ad37bb6d0180bd7a", 
        `strict-transport-security` = "max-age=31536000; includeSubDomains; preload", 
        `cf-cache-status` = "DYNAMIC", `set-cookie` = "REDACTED", 
        `x-content-type-options` = "nosniff", `set-cookie` = "REDACTED", 
        server = "cloudflare", `cf-ray` = "923d0a32db32d2a8-FRA", 
        `content-encoding` = "gzip", `alt-svc` = "h3=\":443\"; ma=86400"), redact = character(0), class = "httr2_headers"), 
    body = charToRaw("{\n  \"id\": \"chatcmpl-BDUet9RLbpStVVeCJZC7FCPNEG8xE\",\n  \"object\": \"chat.completion\",\n  \"created\": 1742555815,\n  \"model\": \"gpt-4o-2024-08-06\",\n  \"choices\": [\n    {\n      \"index\": 0,\n      \"message\": {\n        \"role\": \"assistant\",\n        \"content\": \"{\\\"area\\\":120.5}\",\n        \"refusal\": null,\n        \"annotations\": []\n      },\n      \"logprobs\": null,\n      \"finish_reason\": \"stop\"\n    }\n  ],\n  \"usage\": {\n    \"prompt_tokens\": 55,\n    \"completion_tokens\": 8,\n    \"total_tokens\": 63,\n    \"prompt_tokens_details\": {\n      \"cached_tokens\": 0,\n      \"audio_tokens\": 0\n    },\n    \"completion_tokens_details\": {\n      \"reasoning_tokens\": 0,\n      \"audio_tokens\": 0,\n      \"accepted_prediction_tokens\": 0,\n      \"rejected_prediction_tokens\": 0\n    }\n  },\n  \"service_tier\": \"default\",\n  \"system_fingerprint\": \"fp_83df987f64\"\n}\n"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
