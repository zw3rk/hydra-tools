// Tab switching via data attributes.
// Buttons: <button role="tab" data-tab-target="tab-id">
// Panels:  <div role="tabpanel" id="tab-id">
document.addEventListener("click", function(e) {
  var btn = e.target.closest("[role=tab][data-tab-target]");
  if (!btn) return;
  var tablist = btn.closest("[role=tablist]");
  if (!tablist) return;
  // Deactivate all sibling buttons.
  tablist.querySelectorAll("[role=tab]").forEach(function(b) {
    b.classList.remove("active");
  });
  btn.classList.add("active");
  // Hide all sibling panels, show the target.
  var parent = tablist.parentElement;
  parent.querySelectorAll("[role=tabpanel]").forEach(function(p) {
    p.style.display = "none";
  });
  var target = document.getElementById(btn.getAttribute("data-tab-target"));
  if (target) target.style.display = "";
});
