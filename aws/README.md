# ceRt scheduler on AWS

GitHub's scheduler started the daily **2.5–5 hours late** (September 2026: the
00:33 UTC run started around 05:00–06:00), while a run started through GitHub's
API begins within seconds. So a small AWS function starts the workflows on
time. The work itself still runs on GitHub Actions; nothing heavy runs on AWS.

`scheduler.yaml` creates:

| piece | what it does |
| --- | --- |
| a Lambda function, `cert-scheduler` | starts a GitHub workflow on schedule, and runs the **court watcher** (`aws/watcher/watcher.py`) |
| seven schedules (EventBridge Scheduler) | the daily at 00:33, 16:33, 20:33 UTC and Mondays 14:03; the conference reports nightly at 06:00; the site audit 04:00; the court watcher every minute |
| a table, `cert-scheduler-watch` (DynamoDB) | the watcher's memory: what each source has shown, what is pending, and a lease so two polls never overlap |
| a topic, `cert-scheduler-alerts` (SNS) | emails the `AlertEmail` address when something published is not on the site after 45 minutes, or the watcher keeps erroring (alarm `cert-scheduler-errors`) |
| a role, `cert-scheduler-deploy`, and GitHub's identity provider | lets `deploy-watcher.yml` upload the watcher's code with no AWS keys stored anywhere |
| a secret, `cert-scheduler/github-token` | the GitHub token (Secrets Manager) |

**The court watcher.** Every minute on weekdays from 9 a.m. to 6 p.m. Eastern
(every five minutes otherwise) it reads the Court's own pages: the order lists
page, the slip-opinion feed, the opinions-relating-to-orders page, the two
argument feeds (transcripts and audio), for this Term and the last, plus the
Hermes feed as a hint. Anything new starts the daily within a minute or two,
and stays "pending" until a daily that began after it has **succeeded**; a
daily that fails or never gets a runner is re-run after 5, 10, 20, then every
30 minutes. Details and the reasons for each rule are at the top of
`watcher.py`; what each source carries is in `docs/data-sources.md`.

The workflows' own GitHub crons stay on as a fallback. The function never
starts a workflow that is already queued or running, but a GitHub cron that
fires hours late, after AWS's run has finished, does run a second daily; that
costs runner minutes, nothing else.

**Cost:** about $0.60 a month: $0.40 for the secret, $0.10 for the alarm, and
a few cents of DynamoDB. The function (~20,000 short runs a month), the
schedules and the email fit in AWS's free tier.

---

## Upgrading to the court watcher (October 2026)

If the stack already exists from the steps below, this adds the watcher's
pieces. About 10 minutes.

1. **Update the stack.** CloudFormation → `cert-scheduler` → **Update** →
   **Replace existing template** → **Upload a template file** → choose
   `aws/scheduler.yaml` → **Next**.
2. **Parameters:** leave **GitHubToken** and **Repository** as they are. Set
   **AlertEmail** to the address for alerts. Leave
   **CreateGitHubOIDCProvider** at `true` -- unless the AWS account already
   has a GitHub identity provider (IAM → Identity providers lists
   `token.actions.githubusercontent.com`), in which case `false`. → **Next** →
   **Next** → tick the IAM acknowledgment → **Submit**. Wait for
   **UPDATE_COMPLETE**.
3. **Confirm the email.** AWS sends "AWS Notification - Subscription
   Confirmation" to that address; click **Confirm subscription**. No alerts
   arrive until you do.
4. **Hand GitHub the deploy role.** On the stack's **Outputs** tab, copy
   **DeployRoleArn**, then (or give it to Claude):

       gh variable set AWS_DEPLOY_ROLE_ARN --body "arn:aws:iam::...:role/cert-scheduler-deploy"

5. **Deploy the watcher:** `gh workflow run deploy-watcher.yml --ref main`. It
   tests the code, uploads it, and prints the function's handler and state.
   From then on it deploys by itself whenever `aws/watcher/` changes.
6. **Check it.** Lambda → `cert-scheduler` → **Test**, event
   `{"action": "watch", "force": true}`. The first run records what every source
   shows and starts nothing ("new 0 ..."); later runs log what was new, what
   is pending and whether a daily was started.

Until step 5, the function keeps running the first watcher (the template's
inline code), now every minute -- harmless, and it still starts the daily on
any Hermes change.

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
- **Change a time:** edit the schedule in `scheduler.yaml`, then CloudFormation →
  `cert-scheduler` → **Update** → **Replace current template** → upload it again.
- **Remove all of it:** CloudFormation → `cert-scheduler` → **Delete**.
- **The function's code** is `aws/watcher/watcher.py`, deployed by
  `deploy-watcher.yml`. The `ZipFile` block in `scheduler.yaml` is only the
  bootstrap the stack starts with; CloudFormation rewrites the code only if
  that block's text changes, so after editing it, run `deploy-watcher.yml`.
  (The bootstrap is the first watcher, a port of `.github/scripts/court_watch.py`:
  Hermes only, and a dispatched daily counted as done.)
- **Alerts:** an email names each pending item and links the daily's runs.
  It means the Court published something and no daily has succeeded since --
  usually a GitHub Actions outage or a failing daily; look at the latest run.
- **Logs:** Lambda → `cert-scheduler` → **Monitor** → **View CloudWatch logs**.
  Each poll prints one line: new items, how many pending, whether it started a
  daily, and any source that failed to load.
