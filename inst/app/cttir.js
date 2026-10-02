(function () {
  "use strict";
  var dirty = false;
  var focusTarget = null;

  function focusable(el) {
    if (!el) return null;
    if (el.matches("input, select, textarea, button, a[href], summary, [tabindex]")) return el;
    return el.querySelector("input:not([type=hidden]), select, textarea, button, a[href], [tabindex]");
  }

  function focusById(id) {
    var tries = 0;
    (function attempt() {
      var el = document.getElementById(id);
      var target = focusable(el);
      if (target) {
        var details = target.closest("details");
        while (details) {
          details.open = true;
          details = details.parentElement ? details.parentElement.closest("details") : null;
        }
        target.focus();
        if (target.scrollIntoView) target.scrollIntoView({ block: "center" });
        return;
      }
      if (tries++ < 30) setTimeout(attempt, 100);
    })();
  }

  function rememberFocus() {
    var el = document.activeElement;
    if (!el || el === document.body) return null;
    if (el.id) return { id: el.id };
    if (el.name) return { name: el.name, value: el.value };
    return null;
  }

  function restoreFocus(saved) {
    if (!saved) return;
    setTimeout(function () {
      var el = null;
      if (saved.id) el = document.getElementById(saved.id);
      if (!el && saved.name) {
        var candidates = document.getElementsByName(saved.name);
        for (var i = 0; i < candidates.length; i++) {
          if (candidates[i].value === saved.value) el = candidates[i];
        }
      }
      if (el && document.activeElement !== el) el.focus();
    }, 0);
  }

  function translate(messages, lang) {
    document.documentElement.setAttribute("lang", lang);
    var nodes = document.querySelectorAll("[data-i18n]");
    for (var i = 0; i < nodes.length; i++) {
      var key = nodes[i].getAttribute("data-i18n");
      if (Object.prototype.hasOwnProperty.call(messages, key)) nodes[i].textContent = messages[key];
    }
  }

  function init() {
    Shiny.addCustomMessageHandler("cttir-focus", function (msg) { focusById(msg.id); });
    Shiny.addCustomMessageHandler("cttir-i18n", function (msg) { translate(msg.messages || {}, msg.lang || "en"); });
    Shiny.addCustomMessageHandler("cttir-dirty", function (msg) { dirty = !!msg.dirty; });
    Shiny.addCustomMessageHandler("cttir-busy", function (msg) {
      (msg.ids || []).forEach(function (id) {
        var el = document.getElementById(id);
        if (!el) return;
        el.disabled = !!msg.busy;
        el.setAttribute("aria-disabled", msg.busy ? "true" : "false");
      });
    });
    // Keep keyboard focus when the questionnaire re-renders around the control.
    $(document).on("shiny:value", function (event) {
      if (event.target && /questionnaire$/.test(event.target.id || "")) {
        focusTarget = rememberFocus();
        if (focusTarget && !event.target.contains(document.activeElement)) focusTarget = null;
        var saved = focusTarget;
        setTimeout(function () { restoreFocus(saved); }, 0);
      }
    });
    // Move focus to the opened view so keyboard users land on its heading.
    $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function (event) {
      var pane = document.querySelector(event.target.getAttribute("href"));
      var heading = pane ? pane.querySelector("h1") : null;
      if (heading) {
        heading.setAttribute("tabindex", "-1");
        heading.focus();
      }
    });
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest ? event.target.closest("[data-copy-target]") : null;
    if (!button) return;
    var source = document.getElementById(button.getAttribute("data-copy-target"));
    if (source && navigator.clipboard) navigator.clipboard.writeText(source.textContent);
  });

  window.addEventListener("beforeunload", function (event) {
    if (!dirty) return undefined;
    event.preventDefault();
    event.returnValue = "";
    return "";
  });

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    init();
  } else {
    document.addEventListener("DOMContentLoaded", init);
  }
})();
