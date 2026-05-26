window.switchPage = function(pageName) {
    if (window.Shiny) {
        Shiny.setInputValue('nav_page_click', pageName + '::' + Date.now(), {priority: 'event'});
    }
};

window.switchInitialPageFromUrl = function() {
    const params = new URLSearchParams(window.location.search);
    const pageName = params.get('page');
    if (pageName && window.Shiny) {
        window.switchPage(pageName);
    }
};

window.toggleAnalysisPort = function(port) {
    if (window.Shiny) {
        Shiny.setInputValue('toggle_analysis_port', port + '::' + Date.now(), {priority: 'event'});
    }
};

window.selectMatchingTarget = function(port) {
    if (window.Shiny) {
        Shiny.setInputValue('matching_target_click', port + '::' + Date.now(), {priority: 'event'});
    }
};

window.sortRawTable = function(col) {
    if (window.Shiny) {
        Shiny.setInputValue('dt_sort_click', col + '::' + Date.now(), {priority: 'event'});
    }
};

window.rawTableAction = function(actionName) {
    if (window.Shiny) {
        Shiny.setInputValue('dt_action', actionName + '::' + Date.now(), {priority: 'event'});
    }
};

window.runMatchingSimulation = function() {
    if (window.Shiny) {
        Shiny.setInputValue('run_match', Date.now(), {priority: 'event'});
    }
};

window.resetMatchingSimulation = function() {
    if (window.Shiny) {
        Shiny.setInputValue('matching_reset', Date.now(), {priority: 'event'});
    }
};

window.setDashboardMode = function(modeName) {
    if (window.Shiny) {
        Shiny.setInputValue('dashboard_mode_click', modeName + '::' + Date.now(), {priority: 'event'});
    }
};

window.setExploreTab = function(tabName) {
    if (window.Shiny) {
        Shiny.setInputValue('explore_tab_click', tabName + '::' + Date.now(), {priority: 'event'});
    }
};

window.setStrategyTemplate = function(strategyName) {
    if (window.Shiny) {
        Shiny.setInputValue('strategy_template_click', strategyName + '::' + Date.now(), {priority: 'event'});
    }
};

window.weightGroupMap = {
    pressure: ['sl-flow', 'sl-net', 'sl-roll'],
    buffer: ['sl-space', 'sl-netout', 'sl-export'],
    matching: ['sl-source', 'sl-target', 'sl-distance']
};

window.pushWeightInputs = function(ids) {
    if (!window.Shiny) {
        return;
    }
    ids.forEach(id => {
        const el = document.getElementById(id);
        if (el) {
            Shiny.setInputValue(id, parseInt(el.value, 10), {priority: 'event'});
        }
    });
};

window.updateWeightGroupDisplay = function(groupName) {
    const ids = window.weightGroupMap[groupName] || [];
    if (!ids.length || !ids.every(id => document.getElementById(id))) {
        return;
    }
    ids.forEach(id => {
        const val = parseInt(document.getElementById(id).value, 10) || 0;
        const pctEl = document.getElementById(id.replace('sl-', 'pct-'));
        const valEl = document.getElementById(id.replace('sl-', 'val-'));
        if (pctEl) pctEl.textContent = `實質占比: ${val}%`;
        if (valEl) valEl.textContent = val;
    });
};

window.rebalanceTwoWeights = function(firstVal, secondVal, remaining, minVal, maxVal) {
    let a = firstVal;
    let b = secondVal;
    const base = a + b;

    if (base <= 0) {
        a = Math.round(remaining / 2);
        b = remaining - a;
    } else {
        a = Math.round(remaining * a / base);
        b = remaining - a;
    }

    if (a < minVal) {
        a = minVal;
        b = remaining - a;
    }
    if (b < minVal) {
        b = minVal;
        a = remaining - b;
    }
    if (a > maxVal) {
        a = maxVal;
        b = remaining - a;
    }
    if (b > maxVal) {
        b = maxVal;
        a = remaining - b;
    }

    a = Math.max(minVal, Math.min(maxVal, a));
    b = remaining - a;

    if (b < minVal) {
        b = minVal;
        a = remaining - b;
    }
    if (b > maxVal) {
        b = maxVal;
        a = remaining - b;
    }

    return [a, b];
};

window.rebalanceWeightGroup = function(groupName, changedId) {
    const ids = window.weightGroupMap[groupName] || [];
    if (!ids.length || !ids.every(id => document.getElementById(id))) {
        return;
    }

    const minVal = 10;
    const maxVal = 60;
    const total = 100;
    const changedEl = document.getElementById(changedId);
    if (!changedEl) {
        return;
    }

    let changedVal = parseInt(changedEl.value, 10) || minVal;
    changedVal = Math.max(minVal, Math.min(maxVal, changedVal));
    changedEl.value = changedVal;

    const otherIds = ids.filter(id => id !== changedId);
    const firstEl = document.getElementById(otherIds[0]);
    const secondEl = document.getElementById(otherIds[1]);
    if (!firstEl || !secondEl) {
        window.updateWeightGroupDisplay(groupName);
        window.pushWeightInputs(ids);
        return;
    }

    const remaining = total - changedVal;
    const pairVals = window.rebalanceTwoWeights(
        parseInt(firstEl.value, 10) || minVal,
        parseInt(secondEl.value, 10) || minVal,
        remaining,
        minVal,
        maxVal
    );

    firstEl.value = pairVals[0];
    secondEl.value = pairVals[1];

    window.updateWeightGroupDisplay(groupName);
    window.pushWeightInputs(ids);
};

window.refreshAllWeightGroups = function() {
    Object.keys(window.weightGroupMap).forEach(groupName => {
        window.updateWeightGroupDisplay(groupName);
        const ids = window.weightGroupMap[groupName];
        if (ids.every(id => document.getElementById(id))) {
            window.pushWeightInputs(ids);
        }
    });
};

document.addEventListener('DOMContentLoaded', function() {
    const sidebar = document.getElementById('sidebar');
    const overlay = document.getElementById('sidebarOverlay');
    const advBackdrop = document.getElementById('advBackdrop');
    const advBtn = document.getElementById('advBtn');
    const advClose = document.getElementById('advClose');
    const advCancel = document.getElementById('advCancel');
    const advApply = document.getElementById('advApply');

    function openSidebar() {
        sidebar.classList.add('open');
        if (window.innerWidth <= 768) {
            overlay.classList.add('sidebar-visible');
        } else {
            overlay.classList.remove('sidebar-visible');
        }
    }

    function closeSidebar() {
        sidebar.classList.remove('open');
        overlay.classList.remove('sidebar-visible');
    }

    document.querySelectorAll('.js-hamburger').forEach(btn => {
        btn.addEventListener('click', function() {
            if (sidebar.classList.contains('open')) {
                closeSidebar();
            } else {
                openSidebar();
            }
        });
    });

    overlay.addEventListener('click', closeSidebar);

    function openAdv() {
        advBackdrop.classList.add('open');
        document.body.style.overflow = 'hidden';
        setTimeout(function() {
            if (window.refreshAllWeightGroups) {
                window.refreshAllWeightGroups();
            }
        }, 0);
    }

    function closeAdv() {
        advBackdrop.classList.remove('open');
        document.body.style.overflow = '';
    }

    if (advBtn) advBtn.addEventListener('click', openAdv);
    if (advClose) advClose.addEventListener('click', closeAdv);
    if (advCancel) advCancel.addEventListener('click', closeAdv);
    if (advApply) advApply.addEventListener('click', closeAdv);
    if (advBackdrop) {
        advBackdrop.addEventListener('click', function(e) {
            if (e.target === advBackdrop) closeAdv();
        });
    }
    setTimeout(function() {
        if (window.refreshAllWeightGroups) {
            window.refreshAllWeightGroups();
        }
    }, 0);
    if (window.innerWidth > 768) {
        sidebar.classList.add('open');
        overlay.classList.remove('sidebar-visible');
    }
});

document.addEventListener('shiny:connected', function() {
    window.switchInitialPageFromUrl();
    setTimeout(window.switchInitialPageFromUrl, 500);
    setTimeout(window.switchInitialPageFromUrl, 1500);
});
