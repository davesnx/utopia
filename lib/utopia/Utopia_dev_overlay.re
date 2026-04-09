/* Utopia_dev_overlay.re — Browser dev overlay runtime

   Self-initializes when loaded as a module script in dev mode.
   Uses raw DOM APIs (no React dependency) so it works even when
   the main app hydration path is broken.

   The module is only included in development builds via conditional
   compilation in Generated_dune.ml and esbuild.config.mjs. */

/* === Overlay rendering (raw JS) === */

%mel.raw
{|
// --- Overlay DOM helpers ---

var _overlayRoot = null;

function _getOverlayRoot() {
  if (_overlayRoot) return _overlayRoot;
  var existing = document.getElementById('utopia-dev-overlay');
  if (existing) { _overlayRoot = existing; return existing; }
  var el = document.createElement('div');
  el.id = 'utopia-dev-overlay';
  el.style.cssText = 'position:fixed;top:0;left:0;width:100%;height:100%;z-index:999999;pointer-events:none;font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Roboto,monospace;font-size:14px;';
  document.body.appendChild(el);
  _overlayRoot = el;
  return el;
}

function _escapeHtml(str) {
  return String(str).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
}

function _renderOverlay() {
  var root = _getOverlayRoot();
  var buildState = window.__utopia_dev_build_state;
  var runtimeErrors = window.__utopia_dev_runtime_errors || [];

  var hasBuildErrors = buildState && buildState.status === 'failed' && buildState.errors && buildState.errors.length > 0;
  var hasBuildWarnings = buildState && buildState.warnings && buildState.warnings.length > 0;
  var hasRuntimeErrors = runtimeErrors.length > 0;
  var isRebuilding = buildState && buildState.rebuilding;
  var isReconnecting = window.__utopia_dev_reconnecting;

  if (!hasBuildErrors && !hasBuildWarnings && !hasRuntimeErrors && !isRebuilding && !isReconnecting) {
    root.innerHTML = '';
    root.style.pointerEvents = 'none';
    return;
  }

  root.style.pointerEvents = 'auto';
  var html = '<div style="position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.75);display:flex;justify-content:center;align-items:flex-start;padding-top:40px;overflow-y:auto;">';
  html += '<div style="background:#1a1a2e;color:#e0e0e0;border-radius:12px;max-width:800px;width:90%;max-height:calc(100vh - 80px);overflow-y:auto;box-shadow:0 25px 50px rgba(0,0,0,0.5);border:1px solid #333;">';

  // Header
  html += '<div style="padding:16px 20px;border-bottom:1px solid #333;display:flex;justify-content:space-between;align-items:center;">';
  html += '<div style="display:flex;align-items:center;gap:8px;">';
  html += '<span style="font-weight:600;font-size:15px;">utopia dev</span>';
  if (isReconnecting) {
    html += '<span style="background:#3b3b5c;color:#a0a0ff;padding:2px 8px;border-radius:4px;font-size:12px;animation:pulse 1.5s infinite;">reconnecting\u2026</span>';
  } else if (isRebuilding) {
    html += '<span style="background:#3b3b5c;color:#a0a0ff;padding:2px 8px;border-radius:4px;font-size:12px;">rebuilding\u2026</span>';
  }
  html += '</div>';
  if (hasRuntimeErrors && !hasBuildErrors) {
    html += '<button onclick="window.__utopia_dev_runtime_errors=[];_renderOverlay()" style="background:none;border:1px solid #555;color:#aaa;padding:4px 12px;border-radius:4px;cursor:pointer;font-size:12px;">Dismiss</button>';
  }
  html += '</div>';

  // Build errors (expanded)
  if (hasBuildErrors) {
    html += '<div style="padding:16px 20px;">';
    html += '<div style="color:#ff6b6b;font-weight:600;margin-bottom:12px;font-size:13px;text-transform:uppercase;letter-spacing:0.5px;">' + buildState.errors.length + ' Build Error' + (buildState.errors.length > 1 ? 's' : '') + '</div>';
    buildState.errors.forEach(function(err) {
      html += '<div style="background:#2a1a1a;border:1px solid #4a2020;border-radius:8px;padding:12px 16px;margin-bottom:8px;">';
      if (err.location) {
        html += '<div style="color:#ff9999;font-size:12px;margin-bottom:4px;font-family:monospace;">' + _escapeHtml(err.location) + '</div>';
      }
      html += '<div style="color:#ffcccc;white-space:pre-wrap;font-family:monospace;font-size:13px;line-height:1.5;">' + _escapeHtml(err.message) + '</div>';
      html += '</div>';
    });
    html += '</div>';
  }

  // Build warnings (collapsed by default)
  if (hasBuildWarnings) {
    html += '<div style="padding:0 20px 16px;">';
    html += '<details>';
    html += '<summary style="color:#ffd93d;font-weight:600;margin-bottom:8px;font-size:13px;text-transform:uppercase;letter-spacing:0.5px;cursor:pointer;">';
    html += buildState.warnings.length + ' Warning' + (buildState.warnings.length > 1 ? 's' : '');
    html += '</summary>';
    buildState.warnings.forEach(function(warn) {
      html += '<div style="background:#2a2a1a;border:1px solid #4a4a20;border-radius:8px;padding:12px 16px;margin-bottom:8px;">';
      if (warn.location) {
        html += '<div style="color:#ffe066;font-size:12px;margin-bottom:4px;font-family:monospace;">' + _escapeHtml(warn.location) + '</div>';
      }
      html += '<div style="color:#fff3cc;white-space:pre-wrap;font-family:monospace;font-size:13px;line-height:1.5;">' + _escapeHtml(warn.message) + '</div>';
      html += '</div>';
    });
    html += '</details>';
    html += '</div>';
  }

  // Runtime errors
  if (hasRuntimeErrors) {
    html += '<div style="padding:0 20px 16px;">';
    html += '<div style="color:#ff9f43;font-weight:600;margin-bottom:12px;font-size:13px;text-transform:uppercase;letter-spacing:0.5px;">' + runtimeErrors.length + ' Runtime Error' + (runtimeErrors.length > 1 ? 's' : '') + '</div>';
    runtimeErrors.forEach(function(err) {
      html += '<div style="background:#2a1a0a;border:1px solid #4a3020;border-radius:8px;padding:12px 16px;margin-bottom:8px;">';
      html += '<div style="color:#ffb366;font-size:12px;margin-bottom:4px;">[' + _escapeHtml(err.operation) + ']</div>';
      html += '<div style="color:#ffe0cc;white-space:pre-wrap;font-family:monospace;font-size:13px;line-height:1.5;">' + _escapeHtml(err.message) + '</div>';
      if (err.stack) {
        html += '<details style="margin-top:8px;">';
        html += '<summary style="color:#aa8866;font-size:11px;cursor:pointer;">Stack trace</summary>';
        html += '<pre style="color:#998877;font-size:11px;margin-top:4px;white-space:pre-wrap;max-height:200px;overflow-y:auto;">' + _escapeHtml(err.stack) + '</pre>';
        html += '</details>';
      }
      if (err.context) {
        html += '<div style="color:#887766;font-size:11px;margin-top:4px;">' + _escapeHtml(err.context) + '</div>';
      }
      html += '</div>';
    });
    html += '</div>';
  }

  html += '</div></div>';
  root.innerHTML = html;
}

// --- SSE connection ---

function _connectSSE() {
  var source = new EventSource('/_utopia/dev-events');

  source.onmessage = function(event) {
    try {
      var state = JSON.parse(event.data);
      if (state && state.kind === 'build_state') {
        window.__utopia_dev_build_state = state;
        window.__utopia_dev_reconnecting = false;
        _renderOverlay();
      }
    } catch(e) {
      // ignore malformed events
    }
  };

  source.onerror = function() {
    window.__utopia_dev_reconnecting = true;
    _renderOverlay();
  };

  source.onopen = function() {
    var wasConnected = window.__utopia_dev_was_connected;
    window.__utopia_dev_was_connected = true;
    window.__utopia_dev_reconnecting = false;

    if (wasConnected) {
      // Reconnected after a disconnect — wait briefly for state,
      // then reload if the server is healthy
      setTimeout(function() {
        var state = window.__utopia_dev_build_state;
        if (!state || state.status === 'healthy') {
          window.location.reload();
        } else {
          _renderOverlay();
        }
      }, 300);
    }
  };

  window.__utopia_dev_eventsource = source;
}

// --- Global error handlers ---

function _setupGlobalErrorHandlers() {
  window.onerror = function(message, source, lineno, colno, error) {
    if (!window.__utopia_dev_report_error) return;
    window.__utopia_dev_report_error({
      operation: 'global_error',
      message: String(message),
      stack: error && error.stack ? error.stack : null,
      context: source ? source + ':' + lineno + ':' + colno : null
    });
  };

  window.onunhandledrejection = function(event) {
    if (!window.__utopia_dev_report_error) return;
    var reason = event.reason;
    var message = reason instanceof Error ? reason.message : String(reason);
    var stack = reason instanceof Error ? reason.stack : null;
    window.__utopia_dev_report_error({
      operation: 'global_error',
      message: message,
      stack: stack,
      context: null
    });
  };
}

// --- Global error reporting callback ---

window.__utopia_dev_report_error = function(info) {
  if (!window.__utopia_dev_runtime_errors) window.__utopia_dev_runtime_errors = [];
  window.__utopia_dev_runtime_errors.push({
    operation: info.operation || 'unknown',
    message: info.message || 'Unknown error',
    stack: info.stack || null,
    context: info.context || null,
    timestamp: Date.now()
  });
  _renderOverlay();
};

window.__utopia_dev_runtime_errors = [];

// --- Initialize ---

if (typeof window !== 'undefined' && window.__UTOPIA_DEV_MODE__) {
  _getOverlayRoot();
  _setupGlobalErrorHandlers();
  _connectSSE();
}
|};
