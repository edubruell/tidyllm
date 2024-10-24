structure(list(method = "POST", url = "https://api.openai.com/v1/chat/completions", 
    status_code = 200L, headers = structure(list(date = "Thu, 24 Oct 2024 09:04:05 GMT", 
        `content-type` = "application/json", `access-control-expose-headers` = "X-Request-ID", 
        `openai-organization` = "user-j2dlcmauhfkzofrqjehceoc7", 
        `openai-processing-ms` = "322", `openai-version` = "2020-10-01", 
        `strict-transport-security` = "max-age=31536000; includeSubDomains; preload", 
        `x-ratelimit-limit-requests` = "5000", `x-ratelimit-limit-tokens` = "800000", 
        `x-ratelimit-remaining-requests` = "4999", `x-ratelimit-remaining-tokens` = "799970", 
        `x-ratelimit-reset-requests` = "12ms", `x-ratelimit-reset-tokens` = "2ms", 
        `x-request-id` = "req_7f022bfa88cd3a133dff1c5b88eb64ea", 
        `cf-cache-status` = "DYNAMIC", `set-cookie` = "REDACTED", 
        `x-content-type-options` = "nosniff", `set-cookie` = "REDACTED", 
        server = "cloudflare", `cf-ray` = "8d78cc1e8c532c0c-STR", 
        `content-encoding` = "gzip", `alt-svc` = "h3=\":443\"; ma=86400"), class = "httr2_headers"), 
    body = charToRaw("{\n  \"id\": \"chatcmpl-ALo3BFxOMXCtvznpFEFMLOar6yIn7\",\n  \"object\": \"chat.completion\",\n  \"created\": 1729760645,\n  \"model\": \"gpt-4o-2024-08-06\",\n  \"choices\": [\n    {\n      \"index\": 0,\n      \"message\": {\n        \"role\": \"assistant\",\n        \"content\": \"Hello! How can I assist you today?\",\n        \"refusal\": null\n      },\n      \"logprobs\": null,\n      \"finish_reason\": \"stop\"\n    }\n  ],\n  \"usage\": {\n    \"prompt_tokens\": 21,\n    \"completion_tokens\": 9,\n    \"total_tokens\": 30,\n    \"prompt_tokens_details\": {\n      \"cached_tokens\": 0\n    },\n    \"completion_tokens_details\": {\n      \"reasoning_tokens\": 0\n    }\n  },\n  \"service_tier\": \"default\",\n  \"system_fingerprint\": \"fp_a7d06e42a7\"\n}\n"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
