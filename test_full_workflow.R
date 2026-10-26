# =============================================================
# postlink 全流程测试 — Anthropic provider
# 运行方法: source("test_full_workflow.R")
# 前提: 在 postlink 项目根目录下运行，并已设置 ANTHROPIC_API_KEY
# =============================================================

cat("=== postlink 全流程测试 ===\n\n")

# ── 0. 加载包 ────────────────────────────────────────────────
cat("[ 0 ] 加载包...\n")
devtools::load_all(".")
cat("✓ 包加载成功\n\n")

# ── 1. 检查 API key ──────────────────────────────────────────
cat("[ 1 ] 检查 API key...\n")
key <- Sys.getenv("ANTHROPIC_API_KEY")
if (nchar(key) == 0) stop("请先设置 ANTHROPIC_API_KEY 环境变量:\n  Sys.setenv(ANTHROPIC_API_KEY = 'sk-ant-...')")
cat("✓ ANTHROPIC_API_KEY 已设置 (", nchar(key), " 字符)\n\n")

# ── 2. 构造测试数据 ──────────────────────────────────────────
cat("[ 2 ] 构造测试数据 (逻辑回归 + 混合调整)...\n")
set.seed(42)
n <- 200
X <- data.frame(
  age    = rnorm(n, mean = 50, sd = 10),
  female = rbinom(n, 1, 0.5)
)
# 简单线性生成 outcome
lp <- -1 + 0.03 * X$age + 0.2 * X$female
Y  <- rbinom(n, 1, plogis(lp))
cat("✓ 数据生成完成 (n =", n, ", outcome prevalence =", round(mean(Y), 2), ")\n\n")

# ── 3. 拟合模型 ──────────────────────────────────────────────
cat("[ 3 ] 拟合逻辑回归模型...\n")
fit <- glm(Y ~ age + female, data = X, family = binomial())
cat("✓ 模型拟合完成\n")
print(summary(fit)$coefficients)
cat("\n")

# ── 4. adjMixture 调整 ───────────────────────────────────────
cat("[ 4 ] 运行 adjMixture 调整...\n")
result <- tryCatch({
  adjMixture(fit, X, Y)
}, error = function(e) {
  cat("✗ adjMixture 失败:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(result)) {
  cat("✓ adjMixture 完成\n")
  print(result)
  cat("\n")
}

# ── 5. explain() — 用 Anthropic 解释结果 ────────────────────
cat("[ 5 ] 调用 explain() (provider = anthropic)...\n")
explanation <- tryCatch({
  explain(
    result,
    provider    = "anthropic",
    api_key     = key,
    model       = "claude-sonnet-4-6",
    max_tokens  = 512,
    temperature = 0.3
  )
}, error = function(e) {
  cat("✗ explain() 失败:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(explanation)) {
  cat("✓ explain() 成功\n\n")
  cat("--- 返回内容 ---\n")
  cat(explanation, "\n")
  cat("----------------\n\n")
}

# ── 6. generate_report() ────────────────────────────────────
cat("[ 6 ] 调用 generate_report() (provider = anthropic)...\n")
report <- tryCatch({
  generate_report(
    result,
    provider    = "anthropic",
    api_key     = key,
    model       = "claude-sonnet-4-6",
    max_tokens  = 1024,
    temperature = 0.3
  )
}, error = function(e) {
  cat("✗ generate_report() 失败:", conditionMessage(e), "\n")
  NULL
})
if (!is.null(report)) {
  cat("✓ generate_report() 成功\n\n")
  cat("--- 报告内容 ---\n")
  cat(report, "\n")
  cat("----------------\n\n")
}

cat("=== 全流程测试完成 ===\n")
