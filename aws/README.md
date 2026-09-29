# ceRt scheduler on AWS

GitHub's scheduler started the daily **2.5–5 hours late** (September 2026: the
00:33 UTC run started around 05:00–06:00), while a run started through GitHub's
API begins within seconds. So a small AWS function starts the workflows on
time. The work itself still runs on GitHub Actions; nothing heavy runs on AWS.

`scheduler.yaml` creates:

| piece | what it does |
| --- | --- |
| a Lambda function, `cert-scheduler` | starts a GitHub workflow, or checks the Court's Hermes feed |
| seven schedules (EventBridge Scheduler) | the daily at 00:33, 16:33, 20:33 UTC and Mondays 14:03; the conference reports Mondays 06:00; the site audit 04:00; the Hermes watch every 5 minutes |
| a secret, `cert-scheduler/github-token` | the GitHub token (Secrets Manager) |
| a parameter, `/cert-scheduler/watch-state` | the watcher's memory of the feed (Parameter Store) |

The workflows' own GitHub crons stay on as a fallback. A late GitHub start after
AWS has already started the run finds it queued or running, and the function
never starts a workflow that is already queued or running. The Hermes watch
replaces `watch-court.yml`, which held a GitHub runner for 6–10 hours at a time.

**Cost:** about $0.40 a month, for the secret. The function and the schedules
fit easily in AWS's free tier (about 9,000 short runs a month).

---

## Setting it up (about 15 minutes)

### 1. Make a GitHub token

1. On GitHub, click your photo (top right) → **Settings** → **Developer settings**
   (bottom of the left menu) → **Personal access tokens** → **Fine-grained tokens**
   → **Generate new token**.
2. **Token name:** `cert-scheduler`. **Expiration:** the longest offered (up to a year).
3. **Repository access:** *Only select repositories* → `baldrige/ceRt`.
4. **Permissions** → **Repository permissions** → **Actions:** *Read and write*.
   Leave everything else alone.
5. **Generate token**, and copy it. GitHub shows it only once. Don't paste it
   anywhere except step 2 below.

### 2. Create the AWS pieces

1. Sign in at <https://console.aws.amazon.com>. (No account yet? **Create an AWS
   account** on that page; it needs a card, but this costs cents.)
2. Top right, next to your name, set the region to **US East (N. Virginia)**.
3. In the search bar at the top, type **CloudFormation** and open it.
4. **Create stack** → **With new resources (standard)**.
5. **Upload a template file** → **Choose file** → pick `aws/scheduler.yaml` from
   this repository → **Next**.
6. **Stack name:** `cert-scheduler`. **GitHubToken:** paste the token from step 1.
   Leave **Repository** as `baldrige/ceRt`. → **Next**.
7. The next page has nothing to change → **Next**.
8. At the bottom, tick **"I acknowledge that AWS CloudFormation might create IAM
   resources"** → **Submit**.
9. Wait about two minutes, refreshing, until the status reads
   **CREATE_COMPLETE**. (If it says ROLLBACK, open the **Events** tab and send
   the red line to whoever is helping you.)

### 3. Check it works

1. Search for **Lambda** → open the function **cert-scheduler**.
2. **Test** tab → **Event JSON:** replace the contents with `{"action": "watch"}`
   → **Test**.
3. The result should read `"baseline recorded"` the first time and `"no change"`
   after that. That means the function reached the Court's feed and saved its
   state.
4. Optional: to prove it can start a workflow, test with
   `{"workflow": "audit-site.yml"}` and look for a new run under the repository's
   **Actions** tab. (The audit is harmless to run any time.)

Then tell Claude it's working, and `watch-court.yml` can be switched off.

---

## Later

- **The token expires** (you chose when, in step 1). Make a new one the same way,
  then in the AWS console: **Secrets Manager** → `cert-scheduler/github-token` →
  **Retrieve secret value** → **Edit** → paste → **Save**.
- **Logs:** Lambda → `cert-scheduler` → **Monitor** → **View CloudWatch logs**.
  Each run prints what it decided ("no change", "dispatched daily.yml", ...).
- **Change a time:** edit the schedule in `scheduler.yaml`, then CloudFormation →
  `cert-scheduler` → **Update** → **Replace current template** → upload it again.
- **Remove all of it:** CloudFormation → `cert-scheduler` → **Delete**.
- **The function's code** is inside `scheduler.yaml` (the `ZipFile` block), so
  there's only ever one copy. It reproduces `.github/scripts/court_watch.py`'s
  decisions: a first look only records the feed; a change starts the daily
  unless one is queued or running or was started under 10 minutes ago, in which
  case the next poll tries again.
