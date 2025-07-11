# 🌟 Reputation Tokens for Freelancers

A decentralized reputation system for freelancers built on Stacks blockchain using Clarity smart contracts.

## 🎯 Features

- Non-transferable reputation tokens
- Verified gig completion tracking
- Client feedback and rating system
- Tier-based reputation levels
- Stake-based security mechanism

## 🔧 Technical Details

### Contract Functions

#### For Freelancers
- `initialize-freelancer`: Create new freelancer profile
- `create-gig`: Create new gig with required stake
- `complete-gig`: Mark gig as completed

#### For Clients
- `submit-rating`: Submit rating for completed gig (1-5)

#### Read-Only Functions
- `get-freelancer-profile`: View freelancer stats
- `get-gig-details`: View gig information
- `get-average-rating`: Calculate freelancer's average rating

## 🚀 Getting Started

1. Deploy contract using Clarinet
2. Initialize freelancer profile
3. Create gig with required stake
4. Complete work and mark gig as finished
5. Receive client rating
6. Build reputation over time

## 💡 Reputation Tiers

- Tier 1: New freelancers
- Tier 2: Established (3.0+ rating)
- Tier 3: Premium (4.0+ rating)

## ⚠️ Security Features

- Staking requirement for gig creation
- 24-hour rating cooldown
- One rating per client per gig
- Non-transferable reputation tokens
```
