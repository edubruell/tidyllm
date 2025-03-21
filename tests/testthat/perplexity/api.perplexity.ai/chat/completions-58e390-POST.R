structure(list(method = "POST", url = "https://api.perplexity.ai/chat/completions", 
    status_code = 200L, headers = structure(list(date = "Thu, 20 Mar 2025 08:59:03 GMT", 
        `content-type` = "application/json", `cf-cache-status` = "DYNAMIC", 
        `set-cookie` = "REDACTED", `strict-transport-security` = "max-age=15552000; includeSubDomains; preload", 
        server = "cloudflare", `cf-ray` = "923402dc3d262c0d-STR", 
        `content-encoding` = "gzip"), class = "httr2_headers"), 
    body = charToRaw("{\"id\": \"10822d93-7560-4337-a207-5955ba78ef8a\", \"model\": \"sonar\", \"created\": 1742461143, \"usage\": {\"prompt_tokens\": 8, \"completion_tokens\": 17, \"total_tokens\": 25}, \"citations\": [\"https://web.stanford.edu/class/archive/cs/cs224n/cs224n.1224/reports/custom_116767424.pdf\", \"https://www.poetrysoup.com/poems/perplexity\", \"https://web.stanford.edu/class/archive/cs/cs224n/cs224n.1234/final-reports/final-report-169444285.pdf\", \"https://www.youtube.com/watch?v=7DGlbqHqUd8\", \"https://www.anthropic.com/customers/perplexity\"], \"object\": \"chat.completion\", \"choices\": [{\"index\": 0, \"finish_reason\": \"stop\", \"message\": {\"role\": \"assistant\", \"content\": \"Mind in puzzled dance  \\nPerplexity's gentle grasp  \\nUncertainty reigns\"}, \"delta\": {\"role\": \"assistant\", \"content\": \"\"}}]}"), 
    cache = new.env(parent = emptyenv())), class = "httr2_response")
