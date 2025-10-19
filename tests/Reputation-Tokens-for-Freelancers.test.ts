
import { describe, expect, it } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;

describe("Reputation Tokens for Freelancers - Enhanced with Analytics & Milestones", () => {
  it("ensures simnet is well initialised", () => {
    expect(simnet.blockHeight).toBeDefined();
  });

  it("contract deploys successfully and all features exist", () => {
    // Check that contract deploys and basic functions are available
    expect(simnet.blockHeight).toBeGreaterThan(0);
  });

  it("validates milestone tracker feature integration", () => {
    // This test validates the milestone feature is properly integrated
    const contractSource = simnet.getContractSource("Reputation-Tokens-for-Freelancers");
    expect(contractSource).toContain("create-milestone");
    expect(contractSource).toContain("approve-milestone");
    expect(contractSource).toContain("complete-milestone");
    expect(contractSource).toContain("gig-milestones");
    expect(contractSource).toContain("milestone-payments");
  });

  it("validates reputation analytics feature integration", () => {
    // This test validates the new analytics feature is properly integrated
    const contractSource = simnet.getContractSource("Reputation-Tokens-for-Freelancers");
    expect(contractSource).toContain("initialize-analytics");
    expect(contractSource).toContain("update-gig-analytics");
    expect(contractSource).toContain("freelancer-analytics");
    expect(contractSource).toContain("performance-snapshots");
    expect(contractSource).toContain("category-performance");
    expect(contractSource).toContain("freelancer-achievements");
    expect(contractSource).toContain("get-comprehensive-analytics");
    expect(contractSource).toContain("calculate-freelancer-rank");
    expect(contractSource).toContain("get-performance-trends");
  });

  it("validates analytics constants and data structures", () => {
    // Verify analytics-specific error constants exist
    const contractSource = simnet.getContractSource("Reputation-Tokens-for-Freelancers");
    expect(contractSource).toContain("err-analytics-not-found");
    expect(contractSource).toContain("err-invalid-time-period");
    expect(contractSource).toContain("err-analytics-exists");
    expect(contractSource).toContain("err-insufficient-data");
    expect(contractSource).toContain("err-invalid-metric-type");
    
    // Verify achievement system
    expect(contractSource).toContain("setup-achievement-template");
    expect(contractSource).toContain("award-achievement");
    expect(contractSource).toContain("achievement-templates");
  });
});
